/*---------------------------------------------------------------------------
Copyright 2017 Daan Leijen, Microsoft Corporation.

This is free software; you can redistribute it and/or modify it under the
terms of the Apache License, Version 2.0. A copy of the License can be
found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
//using System;
//using System.Collections.Generic;
//using System.Diagnostics;
//using System.Reflection;

namespace Eff
{
  #region Continuations
  // --------------------------------------------------
  // Continuations
  // --------------------------------------------------

  // Continuations are dynamically typed object to object.
  public interface Cont
  {
  }
  public interface Cont<A> : Cont
  {
    Cont<C> Compose<C>(Fun1<A, C> f);
    Cont<C> Compose<C>(Func<A, C> f);
    Cont<C> ComposeConst<C>(C result);
    Cont<A> ComposeFinally(Fun0<Unit> onFinal, Fun0<Unit> reInit);
    Cont<A> ComposeCatch(Fun1<Exception, A> onExn, bool catchAll );
    Cont<R> ComposeTailResume<R,HB>(Handler<HB> handler, TailResume<R,HB> resume, int skip);
  }

  public abstract class Cont<A, B> : Cont<B>
  {
    public abstract B Resume(A arg, Exception exception = null);

    public Cont<C> Compose<C>(Fun1<B, C> f) {
      return new ContComposeFun<C>(this, f);
    }
    public Cont<C> Compose<C>(Func<B, C> f) {
      return new ContComposeFunc<C>(this, f);
    }
    public Cont<C> ComposeConst<C>(C result) {
      return new ContComposeConst<C>(this,result);
    }
    public Cont<B> ComposeFinally(Fun0<Unit> onFinal, Fun0<Unit> reInit) {
      return new ContComposeFinally(this, onFinal, reInit);
    }
    public Cont<B> ComposeCatch(Fun1<Exception, B> onExn, bool catchAll) {
      return new ContComposeCatch(this, onExn, catchAll);
    }
    public Cont<A, B> ComposeHandler(Handler<B> h) {
      return new ContComposeHandler<A>(this, h);
    }
    public Cont<A, B> ComposeHandler1<S>(Handler1<S, B> h, S local) {
      return new ContComposeHandler1<A, S>(this, h, local);
    }
    public Cont<R> ComposeTailResume<R, HB>(Handler<HB> handler, TailResume<R,HB> resume, int skip) {
      return new ContComposeTailResume<R, HB>(this as Cont<A, HB>, handler, resume, skip);
    }

    private sealed class ContComposeFun<C> : Cont<A, C>
    {
      Cont<A, B> cont;
      Fun1<B, C> fun;

      public ContComposeFun(Cont<A, B> cont, Fun1<B, C> fun) {
        this.cont = cont;
        this.fun = fun;
      }

      public override C Resume(A arg, Exception exn) {
        return Op.Bind(cont.Resume(arg, exn), fun);
      }
    }

    private sealed class ContComposeFunc<C> : Cont<A, C>
    {
      Cont<A, B> cont;
      Func<B, C> fun;

      public ContComposeFunc(Cont<A, B> cont, Func<B, C> fun) {
        this.cont = cont;
        this.fun = fun;
      }

      public override C Resume(A arg, Exception exn) {
        return Op.Bind(cont.Resume(arg, exn), fun);
      }
    }

    private sealed class ContComposeConst<C> : Cont<A, C>
    {
      Cont<A, B> cont;
      C result;

      public ContComposeConst(Cont<A, B> cont, C result) {
        this.cont = cont;
        this.result = result;
      }

      public override C Resume(A arg, Exception exn) {
        return Op.BindConst(cont.Resume(arg, exn), result);
      }
    }

    private sealed class ContComposeFinally : Cont<A, B>
    {
      Cont<A, B> cont;
      Fun0<Unit> onFinal;
      Fun0<Unit> reInit;
      int resumeCount = 0;

      public ContComposeFinally(Cont<A, B> cont, Fun0<Unit> onFinal, Fun0<Unit> reInit) {
        this.cont = cont;
        this.onFinal = onFinal;
        this.reInit = reInit;
      }

      public override B Resume(A arg, Exception exn) {
        if (resumeCount > 0 && reInit != null) reInit.Call();
        resumeCount++;
        return Op.HandleFinally<B>(new Primitive.FunFunc0<B>(() => cont.Resume(arg, exn)), onFinal, reInit);
      }
    }

    private sealed class ContComposeCatch : Cont<A, B>
    {
      Cont<A, B> cont;
      Fun1<Exception, B> onExn;
      bool catchAll;

      public ContComposeCatch(Cont<A, B> cont, Fun1<Exception, B> onExn, bool catchAll) {
        this.cont = cont;
        this.onExn = onExn;
        this.catchAll = catchAll;
      }

      public override B Resume(A arg, Exception exn) {
        return Op.HandleCatch<B>(new Primitive.FunFunc0<B>(() => cont.Resume(arg, exn)), onExn, catchAll);
      }
    }

    private sealed class ContComposeHandler<R> : Cont<R, B>
    {
      Cont<R, B> cont;
      Handler<B> handler;

      public ContComposeHandler(Cont<R, B> cont, Handler<B> handler) {
        this.cont = cont;
        this.handler = handler;
      }

      public override B Resume(R arg, Exception exn) {
        return handler.HandleResume<R>(cont, arg, exn);
      }
    }

    private sealed class ContComposeHandler1<R, S> : Cont<R, B>
    {
      Cont<R, B> cont;
      Handler1<S, B> handler;
      S local;

      public ContComposeHandler1(Cont<R, B> cont, Handler1<S, B> handler, S local) {
        this.cont = cont;
        this.handler = handler;
        this.local = local;
      }

      public override B Resume(R arg, Exception exn) {
        return handler.HandleResume<R>(cont, arg, local, exn);
      }
    }

    private sealed class ContComposeTailResume<R, HB> : Cont<A, R>
    {
      Cont<A, HB> cont;
      Handler<HB> handler;
      TailResume<R,HB> resume;
      int skip;

      public ContComposeTailResume(Cont<A, HB> cont, Handler<HB> handler, TailResume<R,HB> resume, int skip) {
        Debug.Assert(cont != null);
        this.cont = cont;
        this.handler = handler;
        this.skip = skip;
        this.resume = resume;
      }

      public override R Resume(A arg, Exception exn) {
        return handler.HandleTailResume<R, A>(cont, arg, exn, resume, skip);
      }
    }
  }

  sealed class ContId<A> : Cont<A, A>
  {
    public static ContId<A> id = new ContId<A>();

    public override A Resume(A arg, Exception exception) {
      if (exception != null) {
        throw exception;
      }
      else {
        return arg;
      }
    }
  }

  sealed class ContAction<A> : Cont<object, A>
  {
    private Fun0<A> action;

    public ContAction(Fun0<A> action) {
      this.action = action;
    }
    public ContAction(Func<A> action) {
      this.action = new Primitive.FunFunc0<A>(action);
    }
    public override A Resume(object arg, Exception exception) {
      if (exception != null) {
        throw exception;
      }
      else {
        return action.Call();
      }
    }
  }


  class Id<A> : Fun1<A, A>
  {
    public static Id<A> singleton = new Id<A>();
    public object Apply(A arg) {
      return arg;
    }
  }
  #endregion

  #region Bind and Yield

  // --------------------------------------------------
  // (ect and Operation are just convenience
  // --------------------------------------------------

  public class Effect
  {
    public readonly string EffectTag;
    private List<Operation> operations;

    public Effect(string effectTag) {
      EffectTag = effectTag;
      operations = new List<Operation>();
    }
    public Operation<A, B> NewOp<A, B>(string opName) {
      Operation<A, B> op = new Operation<A, B>(this, opName, operations.Count);
      operations.Add(op);
      return op;
    }
  }

  public class Operation
  {
    public readonly Effect Effect;
    public readonly int OpTag;
    public readonly string OpName;

    public Operation(Effect effect, string opName, int opTag) {
      Effect = effect;
      OpTag = opTag;
      OpName = opName;
    }
  }

  public class Operation<A, B> : Operation
  {
    public Operation(Effect effect, string opName, int opTag) : base(effect, opName, opTag) { }
    public B Yield(A arg) {
      return Op.YieldOp<A, B>(Effect.EffectTag, OpName, OpTag, arg);
    }
  }

  // --------------------------------------------------
  // Bind and Yield
  // --------------------------------------------------
  public abstract class YieldPoint
  {
    private readonly Handler handler;

    public YieldPoint(Handler handler) {
      this.handler = handler;
    }

    public bool HandledBy(Handler handler) {
      return (this.handler == handler);
    }

    public abstract B CallBranch<B>(Handler<B> handler, Cont<B> cont, out Resume resume);
    public abstract Cont<B> HandlerCompose<B>(Handler<B> handler, Cont<B> cont);
  }

  // The `YieldPoint` class connects the types at the point of the
  // yield, namely operation `O` and result `R`, with the type of the branch
  // with result `B` and the type of the handler. At the `handler.CallBranch`
  // all types will be resolved.
  public sealed class YieldPoint<O, R> : YieldPoint
  {
    Branch branch;
    O op;

    public YieldPoint(Handler handler, Branch branch, O op) : base(handler) {
      this.branch = branch;
      this.op = op;
    }

    public override B CallBranch<B>(Handler<B> handler, Cont<B> cont, out Resume resume) {
      Debug.Assert(HandledBy(handler));
      return handler.CallBranch<O, R>((IBranch<O, R, B>)branch, (Cont<R,B>)cont, op, out resume);
    }

    public override Cont<B> HandlerCompose<B>(Handler<B> handler, Cont<B> cont) {
      Debug.Assert(!HandledBy(handler));
      return handler.ContCompose<R>((Cont<R, B>)cont);
    }
  }

  public sealed class YieldPointReturn<A> : YieldPoint
  {
    A result;

    public YieldPointReturn(Handler handler, A result) : base(handler) {
      this.result = result;
    }

    public override B CallBranch<B>(Handler<B> handler, Cont<B> cont, out Resume resume) {
      Debug.Assert(HandledBy(handler));
      resume = ReturnResume<A,B>.Singleton;
      return ((Handler<A, B>)handler).CallReturnFun(result);
    }

    public override Cont<B> HandlerCompose<B>(Handler<B> handler, Cont<B> cont) {
      Debug.Assert(!HandledBy(handler));
      return cont;
    }
  }

  public static class Op
  {
    public static Cont yieldCont = null;
    public static YieldPoint yieldPoint = null;
    public static ResumeKind yieldResumeKind = ResumeKind.Never;
    public static bool yielding = false;

    public static B Bind<A, B>(A x, Fun1<A, B> next) {
      if (yielding) {
        yieldCont = ((Cont<A>)yieldCont).Compose<B>(next);
        return default(B);
      }
      else {
        return next.Call(x);
      }
    }


    // Convenience
    public static B Bind<A, B>(A x, Func<A, B> next) {
      if (yielding) {
        yieldCont = ((Cont<A>)yieldCont).Compose<B>(next);
        return default(B);
      }
      else {
        return next(x);
      }
    }

    public static B BindConst<A, B>(A x, B result) {
      if (yielding) {
        yieldCont = ((Cont<A>)yieldCont).ComposeConst<B>(result);
        return default(B);
      }
      else {
        return result;
      }
    }

    private static int tailYieldCount = 0;
    private static int tailYieldMax = 100;

    public static R YieldOp<O, R>(string effectTag, string opName, int opTag, O op) {
      Debug.Assert(!yielding);
      int skip;
      Branch branch;
      Handler handler;
      Handler.Find(effectTag, opName, opTag, out handler, out branch, out skip);
      yieldResumeKind = branch.ResumeKind;
      if ((yieldResumeKind <= ResumeKind.Tail && tailYieldCount < tailYieldMax) || handler.IsLinear) {
        // invoke directly using skip frames; this is the case 95% of the time and should
        // be optimized well (e.g. there should be no allocation along this path).
        if (!handler.IsLinear) tailYieldCount++;
        return handler.HandleTailBranch<O, R>(branch, op, skip);  // works for Never too and thus for linear handlers
      }
      else {
        // yield normally 
        yieldPoint = new YieldPoint<O, R>(handler, branch, op);
        tailYieldCount = 0;
        yieldCont = ContId<R>.id;
        yielding = true;
        return default(R);
      }
    }

    public static R YieldOpX1<O, R, E>(string effectTag, string opName, int opTag, O op) {
      Debug.Assert(!yielding);
      int skip;
      Branch branch;
      Handler handler;
      Handler.Find(effectTag, opName, opTag, out handler, out branch, out skip);
      branch = ((IBranchX1)branch).TypeApply<O, R, E>();
      yieldResumeKind = branch.ResumeKind;
      if ((yieldResumeKind <= ResumeKind.Tail && tailYieldCount < tailYieldMax) || handler.IsLinear) {
        // invoke directly using skip frames; this is the case 95% of the time and should
        // be optimized well (e.g. there should be no allocation along this path).
        if (!handler.IsLinear) tailYieldCount++;
        return handler.HandleTailBranch<O, R>(branch, op, skip);
      }
      else {
        // yield normally 
        yieldPoint = new YieldPoint<O, R>(handler, branch, op);
        tailYieldCount = 0;
        yieldCont = ContId<R>.id;
        yielding = true;
        return default(R);
      }
    }

    public static B YieldReturn<A, B>(Handler<A,B> handler, A result) {
      yieldPoint = new YieldPointReturn<A>(handler, result);
      yieldResumeKind = ResumeKind.Never;
      yieldCont = null;
      yielding = true;
      return default(B);
    }


    public static A HandleExn<A>(Fun0<A> action, Fun1<Exception, A> onExn, Fun0<Unit> onFinal) {
      if (onExn == null) {
        return HandleFinally<A>(action, onFinal);
      }
      else if (onFinal == null) {
        return HandleCatch<A>(action, onExn);
      }
      else {
        return HandleFinally<A>(new Primitive.FunFunc0<A>(() => {
          return HandleCatch<A>(action, onExn);
        }), onFinal);
      }
    }

    public static A HandleCatch<A>(Fun0<A> action, Fun1<Exception, A> onExn, bool catchAll = false ) {
      try {
        A result = action.Call();
        if (yielding) {
          // extend continuation to handle exceptions again on the resume
          yieldCont = ((Cont<A>)yieldCont).ComposeCatch(onExn,catchAll);
        }
        return result;
      }
      catch (LongJumpException) {
        // never catch long jump exceptions
        throw;
      }
      catch (FinalizeException exn) {
        // only handle finalize exceptions if `catchAll` is `true`
        if (!catchAll) throw;
        return onExn.Call(exn.PreserveStackTrace());
      }
      catch (Exception exn) {
        return onExn.Call(exn.PreserveStackTrace());
      }
    }

    public static A HandleFinally<A>(Fun0<A> action, Fun0<Unit> onFinal, Fun0<Unit> reInit = null) {
      A result;
      try {
        result = action.Call();
      }
      catch (LongJumpException) {
        // never finalize on long jump exceptions
        throw;
      }
      catch (Exception exn) {
        // finalize and rethrow
        Unit u = onFinal.Call();
        // check yielding explicitly to avoid using `PreserveStackTrace` in most cases...
        if (yielding) return Bind<Unit,A>(u, (_) => { throw exn.PreserveStackTrace(); });
        throw;
      }
      if (yielding) {
        // extend continuation, don't finalize yet on a yielding operation! (which is why we cannot use `finally`)
        yieldCont = ((Cont<A>)yieldCont).ComposeFinally(onFinal, reInit);
        return result;
      }
      else return BindConst(onFinal.Call(), result);
    }

    public static B RunFinalizers<R, B>(object resume, B result) {
      Resume<R, B> rresume = resume as Resume<R, B>;
      if (rresume != null) {
        return rresume.RunFinalizers(result);
      }
      else {
        return result;
      }
    }

  }


  public static class ExceptionHelper
  {
    private static Action<Exception> preserveStackTrace = null;

    static ExceptionHelper() {
      System.Reflection.MethodInfo preserveInfo = typeof(Exception).GetMethod("InternalPreserveStackTrace", System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic);
      if (preserveInfo != null) preserveStackTrace = (Action<Exception>)Delegate.CreateDelegate(typeof(Action<Exception>), preserveInfo);
    }

    public static Exception PreserveStackTrace(this Exception ex) {
      if (preserveStackTrace != null) preserveStackTrace(ex);
      return ex;
    }
  }


  public class FinalizeException : Exception
  {
    public FinalizeException(String message) : base(message) 
    {
    }
  }

  public class ResumeFinalizeException : FinalizeException
  {
    public ResumeFinalizeException() : base("Internal exception to run finalizers for effect handlers; do not catch this exception.") { }
  }

  public interface IHandlerException<B> 
  {
    bool HandledBy(Handler h);
    B Handle();
  }

  public class HandlerFinalizeException<B> : ResumeFinalizeException, IHandlerException<B>
  {
    private readonly Handler<B> handler;
    private readonly B result;

    public HandlerFinalizeException(Handler<B> handler, B result) { 
      this.handler = handler;
      this.result = result;
    }

    public bool HandledBy(Handler h) {
      return (h == handler);
    }

    public B Handle() {
      return result;
    }
  }

  public class LongJumpException : Exception
  {
    public LongJumpException(string message) : base(message) { }
  }
  
  public abstract class JumpToHandlerException<B> : LongJumpException, IHandlerException<B>
  {
    private readonly Handler<B> handler;

    public JumpToHandlerException(Handler<B> handler) : base("internal: this exception is used internally for effect handlers; do not handle this exception.") {
      this.handler = handler;
    }

    public bool HandledBy(Handler h) {
      return (h == handler);
    }

    public abstract B Handle();
  }

  public class HandlerReturnException<B> : JumpToHandlerException<B>
  {
    private readonly B returnValue;

    public HandlerReturnException(Handler<B> handler, B returnValue) : base(handler) {
      this.returnValue = returnValue;
    }

    public override B Handle() {
      return returnValue;
    }
  }

  public class HandlerThrowException<B> : JumpToHandlerException<B>
  {
    private readonly Exception exception;

    public HandlerThrowException(Handler<B> handler, Exception exception) : base(handler) {
      this.exception = exception;
    }

    public override B Handle() {
      throw exception.PreserveStackTrace();
    }
  }

  #endregion

  #region Branches
  // --------------------------------------------------
  // Branches
  // --------------------------------------------------

  public enum ResumeKind
  {
    Never,
    Tail,
    ScopedOnce,
    Scoped,
    Once,
    Normal
  }

  public enum HandlerKind
  {
    Deep,
    Linear,
    Shallow
  }

  // Resume functions 
  public interface Resume
  {
    bool HasResumed { get; }
    bool HasFinalized { get; }
  }

  
  public class Resume<R, B> : Resume
  {
    protected bool resumed = false;
    protected bool finalized = false;
    protected Cont<R, B> cont;

    public bool HasResumed { get { return resumed; } }
    public bool HasFinalized { get { return finalized; } }
    
    public virtual B RunFinalizers(B result) {
      finalized = true;
      if (resumed || cont == null) return result;
      resumed = true;
      Exception exn = new ResumeFinalizeException();
      return Op.HandleCatch<B>(
        new Primitive.FunFunc0<B>( () => cont.Resume(default(R), exn) ),
        new Primitive.FunFunc1<Exception,B>( (Exception e) => { if (e == exn) return result; else throw e; }),
        true  // catch finalize exceptions (to catch our own exception)
      );
    }

    public Resume(Cont<R,B> cont) {
      this.cont = cont;
    }

  }

  public class ReturnResume<R,B> : Resume<R,B>
  {
    public static ReturnResume<R, B> Singleton = new ReturnResume<R, B>();
    public ReturnResume() : base(null) {
    }
  }

  public abstract class Resume0<R,B> : Resume<R,B>, Fun1<R,B>
  {
    public Resume0(Cont<R,B> cont = null) : base(cont) { }

    public abstract B Call(R arg);
    
    public object Apply(R arg) {
      //Debug.Assert(!resumed);
      resumed = true;
      return Call(arg);
    }  
  }

  public abstract class Resume1<S, R, B> : Resume<R,B>, Fun2<R, S, B>
  {
    public Resume1(Cont<R, B> cont = null) : base(cont) { }

    public abstract B Call(R arg, S local);

    public object Apply(R arg, S local) {
      //Debug.Assert(!resumed);
      resumed = true;
      return Call(arg,local);
    }
  }

  public interface TailResume<R,B> : Resume
  {
    void Clear();
    R GetResumeArg(Handler<B> h);
  }

  public abstract class Branch
  {
    public readonly ResumeKind ResumeKind;
    public readonly string OpName;

    public Branch(ResumeKind resumeKind, string OpName) {
      ResumeKind = resumeKind;
      this.OpName = OpName;
    }
  }

  // A branch in a handler with result type `B`.
  public abstract class Branch<B> : Branch
  {
    public Branch(ResumeKind resumeKind, string opName) : base(resumeKind, opName) { }
  }

  public interface IBranch<O, R, B> { }

  // Branches without local state
  public sealed class Branch<O, R, B> : Branch<B>, IBranch<O, R, B>
  {
    Fun2<Fun1<R, B>, O, B> branchFun;

    public Branch(ResumeKind resumeKind, string opName,
                    Fun2<Fun1<R, B>, O, B> branchFun) : base(resumeKind, opName) {
      this.branchFun = branchFun;
    }

    public B Call(Fun1<R, B> resume, O op) {
      return branchFun.Call(resume, op);
    }

    // Convenience: wrap into C# functions
    public Branch(ResumeKind resumeKind, string opName,
                    Func<Fun1<R, B>, O, B> branchFun) : base(resumeKind, opName) {
      this.branchFun = new Primitive.FunFunc2<Fun1<R, B>, O, B>(branchFun);
    }

    public Branch(ResumeKind resumeKind, string opName, Func<Func<R, B>, O, B> branchFun) : base(resumeKind, opName) {
      this.branchFun = new Primitive.FunFunc2<Fun1<R, B>, O, B>((Fun1<R, B> r, O op) => {
        return branchFun((arg) => r.Call(arg), op);
      });
    }

  }

  public interface IBranchX1
  {
    Branch TypeApply<O, R, E>();
  }

  public sealed class BranchX1<B> : Branch<B>, IBranchX1
  {
    TypeFun1 branchFun;

    public BranchX1(ResumeKind resumeKind, string opName, TypeFun1 branchFun) : base(resumeKind, opName) {
      this.branchFun = branchFun;
    }

    public Branch TypeApply<O, R, E>() {
      object ofun = branchFun.TypeApply<E>();
      Fun2<Fun1<R, B>, O, B> fun = ofun as Fun2<Fun1<R, B>, O, B>;
      if (fun == null) {
        // convenience; allow a delegate as a resume function
        Func<Fun1<R, B>, O, B> ffun = ofun as Func<Fun1<R, B>, O, B>;
        if (ffun != null) {
          return new Branch<O, R, B>(ResumeKind, OpName, ffun);
        }
        fun = (Fun2<Fun1<R, B>, O, B>)ofun; // cause an error
      }
      return new Branch<O, R, B>(ResumeKind, OpName, fun);
    }

  }


  // Branches on local state
  public abstract class Branch1<S, B> : Branch<B>
  {
    public Branch1(ResumeKind rk, string opName) : base(rk, opName) { }
  }

  public sealed class Branch1<S, O, R, B> : Branch1<S, B>, IBranch<O, R, B>
  {
    Fun3<Fun2<R, S, B>, O, S, B> branchFun;

    public Branch1(ResumeKind resumeKind, string opName,
                    Fun3<Fun2<R, S, B>, O, S, B> branchFun) : base(resumeKind, opName) {
      this.branchFun = branchFun;
    }

    public B Call(Fun2<R, S, B> resume, O op, S local) {
      return branchFun.Call(resume, op, local);
    }

    // Convenience: wrap into C# functions
    public Branch1(ResumeKind resumeKind, string opName,
                    Func<Fun2<R, S, B>, O, S, B> branchFun) : base(resumeKind, opName) {
      this.branchFun = new Primitive.FunFunc3<Fun2<R, S, B>, O, S, B>(branchFun);
    }

    public Branch1(ResumeKind resumeKind, string opName, Func<Func<R, S, B>, O, S, B> branchFun) : base(resumeKind, opName) {
      this.branchFun = new Primitive.FunFunc3<Fun2<R, S, B>, O, S, B>((Fun2<R, S, B> r, O op, S local) => {
        return branchFun((R arg, S loc) => r.Call(arg, loc), op, local);
      });
    }

  }

  public sealed class Branch1X1<S, B> : Branch1<S, B>, IBranchX1
  {
    TypeFun1 branchFun;

    public Branch1X1(ResumeKind resumeKind, string opName, TypeFun1 branchFun) : base(resumeKind, opName) {
      this.branchFun = branchFun;
    }

    public Branch TypeApply<O, R, E>() {
      object ofun = branchFun.TypeApply<E>();
      Fun3<Fun2<R, S, B>, O, S, B> fun = ofun as Fun3<Fun2<R, S, B>, O, S, B>;
      if (fun == null) {
        // convenience; allow a delegate as a resume function
        Func<Fun2<R, S, B>, O, S, B> ffun = ofun as Func<Fun2<R, S, B>, O, S, B>;
        if (ffun != null) {
          return new Branch1<S, O, R, B>(ResumeKind, OpName, ffun);
        }
        fun = (Fun3<Fun2<R, S, B>, O, S, B>)ofun; // cause an error
      }
      return new Branch1<S, O, R, B>(ResumeKind, OpName, fun);
    }

  }

  #endregion

  #region Abstract Handlers
  // --------------------------------------------------
  // Handlers
  // --------------------------------------------------

  public abstract class Handler
  {
    private static Handler[] handlers = new Handler[32];
    private static int top = -1;

    public readonly string effectTag;
    protected readonly Branch[] branches;
    protected int skip = 0;

    protected readonly HandlerKind handlerKind;
    public bool IsLinear { get { return (handlerKind == HandlerKind.Linear); } }
    public bool IsShallow { get { return (handlerKind == HandlerKind.Shallow); } }

    // Used to directly call a branch if it is tail resumptive.
    public abstract R HandleTailBranch<O, R>(Branch branch, O op, int skip);

    public Handler(string effectTag, Branch[] branches, HandlerKind handlerKind = HandlerKind.Deep) {
      this.effectTag = effectTag;
      this.handlerKind = handlerKind;
      this.branches = branches;
      /*
      if (branches == null) {
        this.branches = null;
      }
      else {
        // ensure the op tags match their index
        this.branches = new Branch[branches.Length];
        foreach (Branch branch in branches) {
          this.branches[branch.OpTag] = branch;
        };
      }
      */
    }

    public static Handler Top {
      get {
        for (int i = top; i >= 0; i--) {
          Handler h = handlers[top];
          if (h.skip == 0) return h;  // never return  skip handlers
          i -= h.skip;
        }
        return null;
      }
    }

    protected void Pop() {
      if (top >= 0) {
        top--;
        Debug.Assert(this == handlers[top + 1]);
      }
    }

    protected void Push() {
      top++;
      if (top >= handlers.Length) {
        int newlen = (handlers.Length <= 1024 ? handlers.Length * 2 : handlers.Length + 1024);
        if (newlen > 1024 * 1024 * 1024) throw new Exception("Handler stack has grown too large");
        Handler[] newHandlers = new Handler[newlen];
        handlers.CopyTo(newHandlers, 0);
        handlers = newHandlers;
      }
      handlers[top] = this;
    }


    public static void Find(string effectTag, string opName, int opTag, out Handler handler, out Branch branch, out int skip) {
      handler = null;
      branch = null;
      for (int i = top; i >= 0; i--) {
        handler = handlers[i];
        if (handler.effectTag == effectTag) {
          branch = handler.branches[opTag];
          skip = (top - i + 1);
          if (branch == null) throw new Exception("Bad handler: no operation handler for: " + effectTag + "/" + opName);
          Debug.Assert(branch.OpName == null || branch.OpName == opName);
          return;
        }
        i -= handler.skip;
      }
      throw new Exception("No handler found for: " + effectTag + "/" + opName);
    }

    // Return a new (cached) skip handler
    public static Handler Skip(int skip) {
      if (skip <= 1) {
        Handler h = Top;
        h.Pop();
        return h;
      }
      else {
        Handler h = SkipHandler.CreateCached(skip);
        h.Push();
        return h;
      }
    }

    public void UnSkip(int skip) {
      if (skip <= 1) {
        // we saved the top handler, now push it back
        Push();
      }
      else {
        // we pushed a skip frame, now pop it
        Debug.Assert(this.skip == skip);
        Pop();
      }
    }

    // Special skip handlers are used for efficient tail resumptions and skip a portion of 
    // the handler stack
    private sealed class SkipHandler : Handler
    {
      private static SkipHandler Skip1 = new SkipHandler(1);
      private static SkipHandler Skip2 = new SkipHandler(2);
      private static SkipHandler Skip3 = new SkipHandler(3);
      private static SkipHandler Skip4 = new SkipHandler(4);

      public static SkipHandler CreateCached(int skip) {
        if (skip <= 1) return Skip1;
        else if (skip == 2) return Skip2;
        else if (skip == 3) return Skip3;
        else if (skip == 4) return Skip4;
        else return new SkipHandler(skip);
      }

      private SkipHandler(int skip) : base("<skip>", null) {
        this.skip = skip;
      }

      public override R HandleTailBranch<O, R>(Branch branch, O op, int skip) {
        throw new Exception("Internal error: invoking tail branch on a skip handler");
      }
    }
  }
  #endregion

  #region Handlers returning a result of type B
  // Handlers returning a result of type B
  public abstract class Handler<B> : Handler
  {
    public Handler(string effectTag, Branch[] branches, HandlerKind handlerKind = HandlerKind.Deep) : base(effectTag, branches, handlerKind) { }

    public abstract B CallBranch<O, R>(IBranch<O, R, B> branch, Cont<R,B> cont, O op, out Resume resume);
    public abstract B CallTailBranch<O, R>(IBranch<O, R, B> branch, O op, out TailResume<R,B> tresume);
    public abstract Cont<R, B> ContCompose<R>(Cont<R, B> cont);

    public B HandleResume<R>(Cont<R, B> cont, R arg, Exception exn = null) {
      B x = ResumeCont(cont, arg, exn);
      return HandleYield(x);
    }

    public B ResumeCont<R>(Cont<R, B> cont, R arg, Exception exception = null) {
      Push();
      try {
        return cont.Resume(arg, exception);
      }
      catch (HandlerFinalizeException<B> exn) {
        // if a tail operation finalizes it yields back to the handler
        if (!exn.HandledBy(this)) throw;
        return exn.Handle();
      }
      catch (JumpToHandlerException<B> exn) {
        // if a tail operation is optimized, it might yield back to the handler
        // using a LongJumpException
        if (!exn.HandledBy(this)) throw;
        return exn.Handle();
      }
      finally {
        Pop();
      }
    }
    

    protected B HandleYield(B result) {
      bool tailresumed;
      do {
        tailresumed = false;
        if (Op.yielding) {
          // yielding
          Cont<B> cont = (Cont<B>)Op.yieldCont;
          if (!Op.yieldPoint.HandledBy(this)) {
            // not for us, reyield with an extended continuation
            Op.yieldCont = Op.yieldPoint.HandlerCompose<B>(this, cont);
          }
          else {
            // we handle the operation; reset Op fields for GC
            YieldPoint yieldPoint = Op.yieldPoint;
            ResumeKind rkind = Op.yieldResumeKind;
            Op.yielding = false;
            Op.yieldCont = null;
            Op.yieldPoint = null;

            // invoke the operation branch
            // note: if a never returning operation is yielded, or an exception is raised
            // while not yet resumed, that may mean finalizers are never executed.
            Resume resume;            
            result = yieldPoint.CallBranch<B>(this, cont, out resume);
            tailresumed = (resume.HasResumed && rkind == ResumeKind.Tail);
          }
        }
      }
      while (tailresumed);
      return result;
    }

    public override R HandleTailBranch<O, R>(Branch ybranch, O op, int skip) {
      // push skip handler so operations in the tail branch get handled correctly
      Handler hskip = Handler.Skip(skip);
      TailResume<R,B> resume = null;
      B result;
      try {
        result = CallTailBranch<O, R>((IBranch<O, R, B>)ybranch, op, out resume);
      }
      catch (Exception exn) {
        // Raised exception in the branch
        // we long jump back to the handler and rethrow from there.
        // note: this is done for `LongJumpException`s too.
        resume.Clear();
        throw new HandlerThrowException<B>(this, exn);
      }
      finally {
        hskip.UnSkip(skip);
      }

      // we are yielding; extend the continuation with the skip frame
      if (Op.yielding) {
        // when the continuation is resumed, it calls `HandleTailResume`
        Op.yieldCont = ((Cont<B>)Op.yieldCont).ComposeTailResume<R,B>(this, resume, skip);
        return default(R);
      }
      else if (resume.HasFinalized) {
        resume.Clear();
        throw new HandlerFinalizeException<B>(this, result);
      }
      else if (!resume.HasResumed) {
        // Returned directly from the branch; 
        // we finalize back to the handler and return from there.
        resume.Clear();
        throw new HandlerReturnException<B>(this, result);
      }
      else return resume.GetResumeArg(this); 
    }

    public R HandleTailResume<R, A>(Cont<A, B> cont, A arg, Exception exnarg, TailResume<R,B> resume, int skip) {
      // push skip handler so operations in the tail branch get handled correctly
      Handler hskip = Handler.Skip(skip);
      B result;
      try {
        result = cont.Resume(arg, exnarg);
      }
      catch (Exception exn) {
        // Raised exception in the branch
        // we finalize back to the handler and rethrow from there.
        // note: we do this for `FinalizeException`s too.
        resume.Clear();
        throw new HandlerThrowException<B>(this, exn);
      }
      finally {
        hskip.UnSkip(skip);
      }

      // we are yielding; extend the continuation with the skip frame
      if (Op.yielding) {
        Op.yieldCont = ((Cont<B>)Op.yieldCont).ComposeTailResume<R,B>(this, resume, skip);
        return default(R);
      }
      else if (resume.HasFinalized) {
        resume.Clear();
        throw new HandlerFinalizeException<B>(this, result);
      }
      else if (!resume.HasResumed) {
        // Returned directly from the branch; 
        // we finalize back to the handler and return from there.
        resume.Clear();
        throw new HandlerReturnException<B>(this, result);
      }
      else return resume.GetResumeArg(this);
    }

  }
  #endregion

  #region Generic Handler from A to B
  // --------------------------------------------------
  // Handlers that take an action returning A to a result B
  // --------------------------------------------------

  // Handlers from actions of type A to B.
  public abstract class Handler<A, B> : Handler<B>
  {
    public Handler(string effectTag, Branch[] branches, HandlerKind handlerKind = HandlerKind.Deep) : base(effectTag, branches, handlerKind) { }

    public abstract B CallReturnFun(A arg);

    public B Handle(Fun0<A> action) {
      B result;
      Push();
      try {
        // return executed outside the handler (like branches)
        result = Op.Bind(action.Call(), (A x) => Op.YieldReturn<A,B>(this,x));

        // Or, return executed inside the handler
        // result = Op.Bind(action.Call(), CallReturnFun);  
      }
      catch (HandlerFinalizeException<B> exn) {
        // if a tail operation finalizes
        if (!exn.HandledBy(this)) throw;
        return exn.Handle();
      }
      catch (JumpToHandlerException<B> exn) {
        // if a tail operation returns directly or throws
        if (!exn.HandledBy(this)) throw;
        return exn.Handle();
      }
      finally {
        Pop();
      }
      return HandleYield(result);
    }
  }

  #endregion

  #region Concrete Handler without local state
  // --------------------------------------------------
  // Concrete Handler without local state (non-parameterized)
  // --------------------------------------------------
  public sealed class Handler0<A, B> : Handler<A, B>
  {
    private Fun1<A, B> returnFun;

    public Handler0(string effectTag, Fun1<A, B> returnFun, Branch<B>[] branches, HandlerKind handlerKind = HandlerKind.Deep) : base(effectTag, branches, handlerKind) {
      this.returnFun = returnFun;
    }

    public override B CallReturnFun(A arg) {
      return returnFun.Call(arg);
    }

    public override Cont<R, B> ContCompose<R>(Cont<R, B> cont) {
      return cont.ComposeHandler(this);
    }

    public override B CallBranch<O, R>(IBranch<O, R, B> ybranch, Cont<R,B> cont, O op, out Resume oresume) {
      Branch<O, R, B> branch = (Branch<O, R, B>)ybranch;
      ResumeKind rkind = branch.ResumeKind;
      Resume0<R, B> resume;
      if (rkind == ResumeKind.Never) resume = ResumeNever<R>.singleton;
      else if (rkind == ResumeKind.Tail) resume = new ResumeTail<R>(this, cont);
      else resume = new ResumeNormal<R>(this, cont);
      oresume = resume;
      return branch.Call(resume, op);
    }

    public override B CallTailBranch<O, R>(IBranch<O, R, B> ybranch, O op, out TailResume<R,B> resume) {
      Branch<O, R, B> branch = (Branch<O, R, B>)ybranch;
      Debug.Assert(branch.ResumeKind <= ResumeKind.Tail);
      ResumeTailDirect<R> tresume = ResumeTailDirect<R>.Singleton;
      resume = tresume;
      return branch.Call(tresume, op);
    }

    // Convenience
    public Handler0(string effectTag, Func<A, B> returnFun, Branch<B>[] branches, HandlerKind handlerKind = HandlerKind.Deep) : base(effectTag, branches, handlerKind) {
      this.returnFun = new Primitive.FunFunc1<A, B>(returnFun);
    }

    public static Fun1<Fun0<A>, B> Create(string effectTag, Fun1<A, B> returnFun, Branch<B>[] branches, HandlerKind handlerKind = HandlerKind.Deep) {
      Handler0<A, B> h = new Handler0<A, B>(effectTag, returnFun, branches, handlerKind);
      return new HandlerFun0(h);
    }

    public static Func<Func<A>, B> Create(string effectTag, Func<A, B> returnFun, Branch<B>[] branches, HandlerKind handlerKind = HandlerKind.Deep) {
      Handler0<A, B> h = new Handler0<A, B>(effectTag, returnFun, branches, handlerKind);
      return (action) => h.Handle(new Primitive.FunFunc0<A>(action));
    }

    private sealed class HandlerFun0 : Fun1<Fun0<A>, B>
    {
      Handler0<A, B> handler;
      public HandlerFun0(Handler0<A, B> handler) {
        this.handler = handler;
      }
      public object Apply(Fun0<A> action) {
        return handler.Handle(action);
      }
    }

    private sealed class ResumeNormal<R> : Resume0<R, B>
    {
      private Handler<B> handler;

      public ResumeNormal(Handler<B> handler, Cont<R, B> cont) : base(cont) {
        this.handler = handler;
      }
      
      public override B Call(R arg) {
        return handler.HandleResume<R>(cont, arg);
      }
    }

    private sealed class ResumeTail<R> : Resume0<R, B>
    {
      private Handler<B> handler;

      public ResumeTail(Handler<B> h, Cont<R, B> c) : base(c) {
        handler = h;
      }

      public override B Call(R arg) {
        return handler.ResumeCont<R>(cont, arg);  // do not end with HandleYield but return to the while-tailresume loop
      }
      
    }

    private sealed class ResumeNever<R> : Resume0<R, B>
    {
      public static ResumeNever<R> singleton = new ResumeNever<R>();

      public override B RunFinalizers(B result) {
        throw new Exception("Trying to finalize an operation that was marked as never-resuming"); // + ((Handler)h).effectTag);
      }
      public override B Call(R arg) {
        throw new Exception("Trying to resume an operation that was marked as never-resuming");
      }
    }

    private sealed class ResumeTailDirect<R> : Resume0<R, B>, TailResume<R,B>
    {
      private static ResumeTailDirect<R> singleton = new ResumeTailDirect<R>();
      public static ResumeTailDirect<R> Singleton {
        get {
          singleton.Clear();
          return singleton;
        }
      }

      public void Clear() {
        resumed = false;
        finalized = false;
        resumeArg = default(R);
      }

      public override B RunFinalizers(B result) {
        resumed = true;
        finalized = true;
        return result;
      }

      private R resumeArg;
      
      public R GetResumeArg(Handler<B> h) {
        R arg = resumeArg;
        Clear();
        return arg;     
      }
 
      public override B Call(R arg) {
        resumeArg = arg;
        return default(B);
      }
    }
  }


  #endregion

  #region Concrete Handler with local state
  // --------------------------------------------------
  // Concrete Handler with local state S (parameterized) going from actions A to a result B
  // --------------------------------------------------
  public interface Handler1<S, B>
  {
    B HandleResume<R>(Cont<R, B> cont, R arg, S newlocak, Exception exn = null);
    void SetLocal(S local);
  }

  // Handler with parameterized local state S, going from an action with type A to a result B.
  public sealed class Handler1<S, A, B> : Handler<A, B>, Handler1<S, B>
  {
    private Fun2<A, S, B> returnFun;
    public S local;

    public Handler1(string effectTag, Fun2<A, S, B> returnFun, Branch1<S, B>[] branches, HandlerKind handlerKind = HandlerKind.Deep) : base(effectTag, branches, handlerKind) {
      this.returnFun = returnFun;
      local = default(S);
    }

    private Handler1(string effectTag, Fun2<A, S, B> returnFun, Branch[] branches, S local, HandlerKind handlerKind = HandlerKind.Deep) : base(effectTag, branches,handlerKind) {
      this.returnFun = returnFun;
      this.local = local;
    }

    public B Handle(Fun0<A> action, S local0) {
      Handler1<S, A, B> h = new Handler1<S, A, B>(effectTag, returnFun, branches, local0, handlerKind); // copy for identity
      return h.Handle(action);
    }

    public override Cont<R, B> ContCompose<R>(Cont<R, B> cont) {
      return cont.ComposeHandler1(this, this.local);
    }

    // called from regular resumption
    public void SetLocal(S newLocal) {
      local = newLocal;
    }

    public B HandleResume<R>(Cont<R, B> cont, R arg, S newlocal, Exception exn) {
      SetLocal(newlocal);
      return HandleResume(cont, arg, exn);
    }


    public override B CallReturnFun(A arg) {
      return returnFun.Call(arg, local);
    }

    public override B CallBranch<O, R>(IBranch<O, R, B> ybranch, Cont<R, B> cont, O op, out Resume oresume) {
      Branch1<S, O, R, B> branch = (Branch1<S, O, R, B>)ybranch;
      ResumeKind rkind = branch.ResumeKind;
      Resume1<S, R, B> resume;
      if (rkind == ResumeKind.Never) resume = ResumeNever1<R>.singleton;
      else if (rkind == ResumeKind.Tail) resume = new ResumeTail1<R>(this, cont);
      else resume = new ResumeNormal1<R>(this, cont);
      oresume = resume;
      return branch.Call(resume, op, local);
    }


    public override B CallTailBranch<O, R>(IBranch<O, R, B> ybranch, O op, out TailResume<R,B> resume) {
      Branch1<S, O, R, B> branch = (Branch1<S, O, R, B>)ybranch;
      Debug.Assert(branch.ResumeKind <= ResumeKind.Tail);
      ResumeTailDirect1<R> tresume = ResumeTailDirect1<R>.Singleton;
      resume = tresume;
      return branch.Call(tresume, op, local);
    }

    // Convenience
    public Handler1(string effectTag, Func<A, S, B> returnFun, Branch1<S, B>[] branches, HandlerKind handlerKind = HandlerKind.Deep) : base(effectTag, branches, handlerKind) {
      this.returnFun = new Primitive.FunFunc2<A, S, B>(returnFun);
    }

    public static Fun2<S, Fun0<A>, B> Create(string effectTag, Fun2<A, S, B> returnFun, Branch1<S, B>[] branches, HandlerKind handlerKind = HandlerKind.Deep) {
      Handler1<S, A, B> h = new Handler1<S, A, B>(effectTag, returnFun, branches, handlerKind);
      return new HandlerFun1(h);
    }

    public static Func<S, Func<A>, B> Create(string effectTag, Func<A, S, B> returnFun, Branch1<S, B>[] branches, HandlerKind handlerKind = HandlerKind.Deep) {
      Handler1<S, A, B> h = new Handler1<S, A, B>(effectTag, returnFun, branches, handlerKind);
      return (local, action) => h.Handle(new Primitive.FunFunc0<A>(action), local);
    }


    private sealed class HandlerFun1 : Fun2<S, Fun0<A>, B>
    {
      Handler1<S, A, B> handler;
      public HandlerFun1(Handler1<S, A, B> handler) {
        this.handler = handler;
      }
      public object Apply(S local, Fun0<A> action) {
        return handler.Handle(action, local);
      }
    }

    private sealed class ResumeNormal1<R> : Resume1<S, R, B>
    {
      Handler1<S, A, B> handler;
      
      public ResumeNormal1(Handler1<S, A, B> h, Cont<R, B> c) : base(c) { 
        handler = h;
      }

      public override B Call(R arg, S local) {
        handler.SetLocal(local);
        return handler.HandleResume<R>(cont, arg);
      }
    }

    private sealed class ResumeTail1<R> : Resume1<S, R, B>
    {
      Handler1<S, A, B> handler;

      public ResumeTail1(Handler1<S, A, B> h, Cont<R, B> c) : base(c) {
        handler = h;
      }

      public override B Call(R arg, S local) {
        handler.SetLocal(local);
        return handler.ResumeCont<R>(cont, arg); // Don't call HandleYield but return to the while-tailresume loop
      }
    }

    private sealed class ResumeNever1<R> : Resume1<S, R, B>
    {
      public static ResumeNever1<R> singleton = new ResumeNever1<R>();

      public override B RunFinalizers(B result) {
        throw new Exception("Trying to finalize an operation that was marked as never-resuming"); // + ((Handler)h).effectTag);
      }
      public override B Call(R arg, S local) {
        throw new Exception("Trying to resume an operation that was marked as never-resuming"); // + ((Handler)h).effectTag);
      }
    }

    private sealed class ResumeTailDirect1<R> : Resume1<S, R, B>, TailResume<R,B>
    {
      // not safe for multi threading, use thread static if needed.
      private static ResumeTailDirect1<R> singleton = new ResumeTailDirect1<R>();
      public static ResumeTailDirect1<R> Singleton {
        get {
          singleton.Clear();
          return singleton;
        }
      }

      public void Clear() {
        resumed = false;
        finalized = false;
        resumeArg = default(R);
        local = default(S);
      }


      private R resumeArg = default(R);
      private S local = default(S);

      public override B RunFinalizers(B result) {
        finalized = true;
        return result;
      }

      public R GetResumeArg(Handler<B> h) {
        ((Handler1<S, B>)h).SetLocal(local);
        R arg = resumeArg;
        Clear();
        return arg;
      }

      public override B Call(R arg, S local) {
        //Debug.Assert(!resumed);     
        resumeArg = arg;
        this.local = local;
        return default(B);
      }
    }
  }


  #endregion

}