import * as child_process from 'child_process'

import {
	Logger, logger,
	LoggingDebugSession,
	InitializedEvent, TerminatedEvent, OutputEvent,
  Thread,
} from '@vscode/debugadapter'
import { DebugProtocol } from '@vscode/debugprotocol'
import { EventEmitter } from 'events'
import { KokaConfig } from './workspace'
import { Subject } from 'await-notify'
import * as path from 'path'

/*
 * This interface describes the mock-debug specific launch attributes
 * (which are not part of the Debug Adapter Protocol).
 * The schema for these attributes lives in the package.json of the mock-debug extension.
 * The interface should always match this schema.
 */
interface LaunchRequestArguments extends DebugProtocol.LaunchRequestArguments {
	/** An absolute path to the "program" to debug. */
	program: string
	/** enable logging the Debug Adapter Protocol */
	trace?: boolean
}

export class KokaDebugSession extends LoggingDebugSession {

	// we don't support multiple threads, so we can use a hardcoded ID for the default thread
	private static THREAD_ID = 1

	private _configurationDone = new Subject()

  private _runtime : KokaRuntime
	/**
	 * Creates a new debug adapter that is used for one debug session.
	 * We configure the default implementation of a debug adapter here.
	 */


	public constructor(private readonly config: KokaConfig) {
		super("koka-debug.txt")

		// this debugger uses zero-based lines and columns
		this.setDebuggerLinesStartAt1(false)
		this.setDebuggerColumnsStartAt1(false)

		this._runtime = new KokaRuntime(config)

		// setup event handlers
		this._runtime.on('output', (text, category) => {
			const e: DebugProtocol.OutputEvent = new OutputEvent(`${text}\n`)
			e.body.category = category

			this.sendEvent(e)
		})
		this._runtime.on('end', () => {
			this.sendEvent(new TerminatedEvent())
		})
	}

	/**
	 * The 'initialize' request is the first request called by the frontend
	 * to interrogate the features the debug adapter provides.
	 */
	protected initializeRequest(response: DebugProtocol.InitializeResponse, args: DebugProtocol.InitializeRequestArguments): void {

		// build and return the capabilities of this debug adapter:
		response.body = response.body || {}

		// the adapter implements the configurationDoneRequest.
		response.body.supportsConfigurationDoneRequest = true

		// make VS Code not use 'evaluate' when hovering over source
		response.body.supportsEvaluateForHovers = false

		// make VS Code not show a 'step back' button
		response.body.supportsStepBack = false

		// make VS Code not support data breakpoints
		response.body.supportsDataBreakpoints = false

		// make VS Code not support completion in REPL
		response.body.supportsCompletionsRequest = false
		response.body.completionTriggerCharacters = []

		// make VS Code send cancelRequests
		response.body.supportsCancelRequest = true

		// make VS Code not send the breakpointLocations request
		response.body.supportsBreakpointLocationsRequest = false

		this.sendResponse(response)

    // we request configurations early by sending an 'initializeRequest' to the frontend.
		// The frontend will end the configuration sequence by calling 'configurationDone' request.
		this.sendEvent(new InitializedEvent())
	}

	/**
	 * Called at the end of the configuration sequence.
	 * Indicates that all breakpoints etc. have been sent to the DA and that the 'launch' can start.
	 */
	protected configurationDoneRequest(response: DebugProtocol.ConfigurationDoneResponse, args: DebugProtocol.ConfigurationDoneArguments): void {
		super.configurationDoneRequest(response, args)

		// notify the launchRequest that configuration has finished
		this._configurationDone.notify()
	}

	protected async launchRequest(response: DebugProtocol.LaunchResponse, args: LaunchRequestArguments) {

		// make sure to 'Stop' the buffered logging if 'trace' is not set
		logger.setup(args.trace ? Logger.LogLevel.Verbose : Logger.LogLevel.Stop, false)

		// wait until configuration has finished (and configurationDoneRequest has been called)
		// No configuration of breakpoints etc is currently supported so set a low timeout
		await this._configurationDone.wait(1)

		// start the program in the runtime
		this._runtime.start(args.program)

		this.sendResponse(response)
	}

	protected threadsRequest(response: DebugProtocol.ThreadsResponse): void {

		// debug runtime supports no threads so just return a default thread.
		response.body = {
			threads: [
				new Thread(KokaDebugSession.THREAD_ID, "main thread")
			]
		}
		this.sendResponse(response)
	}


	protected cancelRequest(response: DebugProtocol.CancelResponse, args: DebugProtocol.CancelArguments) {
    this._runtime.cancel()
    response.success = true
    response.message = "cancelled"
    this.sendResponse(response)
	}
}

class KokaRuntime extends EventEmitter {
  
  constructor(private readonly config: KokaConfig) {
    super()
  }
  ps?: child_process.ChildProcess

  public start(file: string){
		const target = this.config.target
		if (target == 'C') {
			console.log(`Executing ${this.config.command} -e ${file} -i${this.config.cwd}`)
			this.ps = child_process.spawn(this.config.command, ['-e', file, `-i${this.config.cwd}`], {cwd: this.config.cwd})
			this.ps.stdout.on('data', (data) => {
				this.emit('output', data.toString().trim(), 'stdout')
			})
			this.ps.stderr.on('data', (data) => {
				this.emit('output', data.toString().trim(), 'stderr')
			})
			this.ps.on('close', (code) => {
				this.emit('end', code)
				this.ps = null
			})
		} else if (target == 'JS' || target == 'WASM'){
      const realTarget = target == 'JS' ? 'jsweb' : 'wasmweb'
			// TODO: Provide config for this, make this the default
			const webBuildDir = path.join(this.config.cwd, 'web', 'build')
			console.log(`Executing ${this.config.command} --target=${realTarget} ${file} -i${this.config.cwd} --outputdir=${webBuildDir}`)
			this.ps = child_process.exec(`${this.config.command} --target=${realTarget} ${file} -i${this.config.cwd} --outputdir=${webBuildDir}`, (exitCode, stdout, stderr) => {
				// TODO: separate output streams for compile versus running?
				if (stdout) {
				  this.emit('output', stdout, 'stdout')
				}
				if (stderr) {
				  this.emit('output', stderr, 'stderr')
				}
				if (exitCode){
					this.emit('output', `Compiler exited with error status ${exitCode}`, 'stderr')
					this.emit('end')
				} else {
					this.emit('output', `Compiler exited succesfully`, 'stdout')
					this.emit('end')
				}
			})
		} else {
			// TODO: Support C#
			this.emit('end')
		}
  }
  public cancel(){
    if (this.ps) {
      this.ps.kill() 
      this.ps = null
    }
  }
}