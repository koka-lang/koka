/*---------------------------------------------------------------------------
Copyright 2017 Daan Leijen, Microsoft Corporation.

This is free software; you can redistribute it and/or modify it under the
terms of the Apache License, Version 2.0. A copy of the License can be
found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
//using System;
//using System.Collections.Generic;
//using System.Diagnostics;

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
    Cont<A> ComposeFinally(Fun0<Unit> onFinal, Fun0<bool> guard, Fun0<Unit> reInit);
    Cont<A> ComposeCatch(Fun1<Exception, A> onExn, Fun1<Exception,bool> guard, bool catchAll );
    Cont<R> ComposeTailResume<R,HB>(Handler<HB> handler, TailResume<R> resume, int skip);
    Cont<A> ComposeRunBranch(Handler<A> handler, YieldPoint yieldPoint, Resume resume);
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
    public Cont<B> ComposeFinally(Fun0<Unit> onFinal, Fun0<bool> guard, Fun0<Unit> reInit) {
      return new ContComposeFinally(this, onFinal, guard, reInit);
    }
    public Cont<B> ComposeCatch(Fun1<Exception, B> onExn, Fun1<Exception, bool> guard, bool catchAll) {
      return new ContComposeCatch(this, onExn, guard, catchAll);
    }
    public Cont<A, B> ComposeHandler(Handler<B> h) {
      return new ContComposeHandler<A>(this, h);
    }
    public Cont<A, B> ComposeHandler1<S>(Handler1<S, B> h, S local) {
      return new ContComposeHandler1<A, S>(this, h, local);
    }
    public Cont<R> ComposeTailResume<R, HB>(Handler<HB> handler, TailResume<R> resume, int skip) {
      return new ContComposeTailResume<R, HB>(this as Cont<A, HB>, handler, resume, skip);
    }
    public Cont<B> ComposeRunBranch(Handler<B> handler, YieldPoint yieldPoint, Resume resume) {
      return new ContComposeRunBranch(this, handler, yieldPoint, resume);
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

    private sealed class ContComposeFinally : Cont<A, B>
    {
      Cont<A, B> cont;
      Fun0<Unit> onFinal;
      Fun0<bool> guard;
      Fun0<Unit> reInit;
      int resumeCount = 0;

      public ContComposeFinally(Cont<A, B> cont, Fun0<Unit> onFinal, Fun0<bool> guard, Fun0<Unit> reInit) {
        this.cont = cont;
        this.onFinal = onFinal;
        this.guard = guard;
        this.reInit = reInit;
      }

      public override B Resume(A arg, Exception exn) {
        if (resumeCount > 0 && reInit != null) reInit.Call();
        resumeCount++;
        return Op.HandleFinally<B>(new Primitive.FunFunc0<B>(() => cont.Resume(arg, exn)), onFinal, guard, reInit);
      }
    }

    private sealed class ContComposeCatch : Cont<A, B>
    {
      Cont<A, B> cont;
      Fun1<Exception, B> onExn;
      Fun1<Exception, bool> guard;
      bool catchAll;

      public ContComposeCatch(Cont<A, B> cont, Fun1<Exception, B> onExn, Fun1<Exception, bool> guard, bool catchAll) {
        this.cont = cont;
        this.onExn = onExn;
        this.guard = guard;
        this.catchAll = catchAll;
      }

      public override B Resume(A arg, Exception exn) {
        return Op.HandleCatch<B>(new Primitive.FunFunc0<B>(() => cont.Resume(arg, exn)), onExn, guard, catchAll);
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
      TailResume<R> resume;
      int skip;

      public ContComposeTailResume(Cont<A, HB> cont, Handler<HB> handler, TailResume<R> resume, int skip) {
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

    private sealed class ContComposeRunBranch : Cont<A,B>
    {
      Cont<A, B> cont;
      Handler<B> handler;
      YieldPoint yieldPoint;
      Resume resume;

      public ContComposeRunBranch(Cont<A,B> cont, Handler<B> handler, YieldPoint yieldPoint, Resume resume) {
        this.cont = cont;
        this.handler = handler;
        this.yieldPoint = yieldPoint;
        this.resume = resume;
      }

      public override B Resume(A arg, Exception exn) {
        return handler.RunBranch(cont, arg, exn, yieldPoint, resume);
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

    public abstract ResumeKind ResumeKind { get; }
    public abstract B CallBranch<B>(Handler<B> handler, Cont<B> cont, out Resume resume);
    public abstract Cont<B> HandlerCompose<B>(Handler<B> handler, Cont<B> cont);
    public abstract B RunFinalizers<B>(Handler<B> handler, Resume resume, FinalizeException<B> exn);
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

    public override ResumeKind ResumeKind { get { return branch.ResumeKind; } }

    public override B CallBranch<B>(Handler<B> handler, Cont<B> cont, out Resume resume) {
      Debug.Assert(HandledBy(handler));
      return handler.CallBranch<O, R>((IBranch<O, R, B>)branch, (Cont<R,B>)cont, op, out resume);
    }

    public override Cont<B> HandlerCompose<B>(Handler<B> handler, Cont<B> cont) {
      Debug.Assert(!HandledBy(handler));
      return handler.ContCompose<R>((Cont<R, B>)cont);
    }

    public override B RunFinalizers<B>(Handler<B> handler, Resume resume, FinalizeException<B> exn) {
      Debug.Assert(HandledBy(handler));
      return ((Resume<R, B>)resume).RunFinalizers(handler, exn);
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
        if (yieldResumeKind != ResumeKind.Never) {
          yieldCont = ((Cont<A>)yieldCont).Compose<B>(next);
        }
        return default(B);
      }
      else {
        return next.Call(x);
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
      if ((yieldResumeKind == ResumeKind.Tail && tailYieldCount < tailYieldMax) || handler.IsLinear) {
        // invoke directly using skip frames; this is the case 95% of the time and should
        // be optimized well (e.g. there should be no allocation along this path).
        if (!handler.IsLinear) tailYieldCount++;
        return handler.HandleTailBranch<O, R>(branch, op, skip);  // works for Never too and thus for linear handlers
      }
      else {
        // yield normally 
        yieldPoint = new YieldPoint<O, R>(handler, branch, op);
        tailYieldCount = 0;
        yieldCont = (yieldResumeKind == ResumeKind.Never ? null : ContId<R>.id);
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
      if ((yieldResumeKind == ResumeKind.Tail && tailYieldCount < tailYieldMax) || handler.IsLinear) {
        // invoke directly using skip frames; this is the case 95% of the time and should
        // be optimized well (e.g. there should be no allocation along this path).
        if (!handler.IsLinear) tailYieldCount++;
        return handler.HandleTailBranch<O, R>(branch, op, skip);
      }
      else {
        // yield normally 
        yieldPoint = new YieldPoint<O, R>(handler, branch, op);
        tailYieldCount = 0;
        yieldCont = (yieldResumeKind == ResumeKind.Never ? null : ContId<R>.id);
        yielding = true;
        return default(R);
      }
    }

    // Convenience
    public static B Bind<A, B>(A x, Func<A, B> next) {
      if (yielding) {
        if (yieldResumeKind != ResumeKind.Never) {
          yieldCont = ((Cont<A>)yieldCont).Compose<B>(next);
        }
        return default(B);
      }
      else {
        return next(x);
      }
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
        }), null, null, onFinal);
      }
    }

    public static A HandleCatch<A>(Fun0<A> action, Fun1<Exception, A> onExn, Fun1<Exception,bool> guard = null, bool catchAll = false ) {
      A result;
      try {
        result = action.Call();
        if (yielding && yieldResumeKind != ResumeKind.Never) {
          // extend continuation to handle exceptions again on the resume
          yieldCont = ((Cont<A>)yieldCont).ComposeCatch(onExn, guard, catchAll);
        }
      }
      catch (FinalizeException exn) {
        // only handle finalize exceptions if `catchAll` is `true`
        if (!catchAll) throw;
        if (guard != null && !guard.Call(exn)) throw;  // rethrow if the guard returns `false`
        result = onExn.Call(exn);
      }
      catch (Exception exn) {
        if (guard != null && !guard.Call(exn)) throw;  // rethrow if the guard returns `false`
        result = onExn.Call(exn);
      }
      return result;
    }

    public static A HandleFinally<A>(Fun0<A> action, Fun0<Unit> onFinal, Fun0<bool> guard = null, Fun0<Unit> reInit = null) {
      A result;
      try {
        result = action.Call();
      }
      catch (Exception) {
        // finalize and rethrow
        if (guard==null || guard.Call()) onFinal.Call();
        throw;
      }
      if (yielding && yieldResumeKind != ResumeKind.Never) {
        // extend continuation, don't finalize yet on a yielding operation! (which is why we cannot use `finally`)
        yieldCont = ((Cont<A>)yieldCont).ComposeFinally(onFinal, guard, reInit);
      }
      else {
        // normal return, or yielding `ResumeKind.Never`
        if (guard == null || guard.Call()) onFinal.Call();
      }
      return result;
    }

  }

  // A guard that ensures that it returns `true` only on the first call.
  // This can be used to have finalizers that run at most once.
  public class GuardOnlyOnce : Fun0<bool>
  {
    bool called = false;

    public object Apply() {
      if (called) return (object)(false);
      called = true;
      return (object)(true);
    }
  }

  public class FinalizeException : Exception
  {
    public FinalizeException(String message) : base(message) 
    {
    }
  }

  public abstract class FinalizeException<B> : FinalizeException
  {
    private readonly Handler handler;

    public FinalizeException(Handler handler) : base("internal: this exception is used to run finalizers on operations; do not handle this exception.") {
      this.handler = handler;
    }

    public bool HandledBy(Handler h) {
      return (h == handler);
    }

    public abstract B Handle();
  }

  public class FinalizeReturnException<B> : FinalizeException<B>
  {
    private readonly B returnValue;

    public FinalizeReturnException(Handler<B> handler, B returnValue) : base(handler) {
      this.returnValue = returnValue;
    }

    public override B Handle() {
      return returnValue;
    }
  }

  public class FinalizeThrowException<B> : FinalizeException<B>
  {
    private readonly Exception exception;

    public FinalizeThrowException(Handler handler, Exception exception) : base(handler) {
      this.exception = exception;
    }

    public override B Handle() {
      throw exception;
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
    Once,
    Normal,
    Shallow
  }

  // Resume functions 
  public interface Resume
  {
    bool HasResumed { get; }
  }

  public abstract class Resume<R, B> : Resume
  {
    protected bool resumed;
    public bool HasResumed { get { return resumed; } }

    protected Cont<R, B> cont;

    public B RunFinalizers(Handler<B> handler, FinalizeException<B> exn) {
      if (!resumed && cont != null) return handler.RunFinalizers<R>(cont, exn);
      else return default(B); // throw an error?
    }

    public Resume(Cont<R,B> cont) {
      resumed = false;
      this.cont = cont;
    }

    public void Release() {
      resumed = true; // pretend to have resumed to prevent running finalizers
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

  public interface TailResume<R> : Resume
  {
    R GetResumeArg(Handler h);
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

    protected readonly bool linear;
    public bool IsLinear { get { return linear; } }

    // Used to directly call a branch if it is tail resumptive.
    public abstract R HandleTailBranch<O, R>(Branch branch, O op, int skip);

    public Handler(string effectTag, Branch[] branches, bool linear = false) {
      this.effectTag = effectTag;
      this.linear = linear;
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
  public struct TailResume<R, B>
  {
    public R tailResumeArg;
    public B result;
    public bool resumed;

    public TailResume(bool resumed, B result = default(B), R tailResumeArg = default(R)) {
      this.resumed = resumed;
      this.result = result;
      this.tailResumeArg = tailResumeArg;
    }
  }

  public struct BranchResult<B>
  {
    public B result;
    public bool resumed;

    public BranchResult(bool resumed, B result) {
      this.resumed = resumed;
      this.result = result;
    }
  }

  // Handlers returning a result of type B
  public abstract class Handler<B> : Handler
  {
    public Handler(string effectTag, Branch[] branches, bool linear = false) : base(effectTag, branches, linear) { }

    public abstract B CallBranch<O, R>(IBranch<O, R, B> branch, Cont<R,B> cont, O op, out Resume resume);
    public abstract B CallTailBranch<O, R>(IBranch<O, R, B> branch, O op, out TailResume<R> tresume);
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
      catch (FinalizeException<B> exn) {
        // if a tail operation is optimized, it might yield back to the handler
        // using a FinalizeException
        if (!exn.HandledBy(this)) throw;
        return exn.Handle();
      }
      finally {
        Pop();
      }
    }

    public B RunFinalizers<R>(Cont<R, B> cont, FinalizeException<B> exception) {
      try {
        // raise the exception at the continuation point to run finalizers
        return cont.Resume(default(R), exception);
      }
      catch (FinalizeException<B> exn) {
        if (!exn.HandledBy(this)) throw;
        return exn.Handle();
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
            if (Op.yieldResumeKind != ResumeKind.Never) {
              Op.yieldCont = Op.yieldPoint.HandlerCompose<B>(this, cont);
            }
          }
          else {
            // we handle the operation; reset Op fields for GC
            Op.yielding = false;
            Op.yieldCont = null;
            YieldPoint yieldPoint = Op.yieldPoint;
            Op.yieldPoint = null;

            // invoke the operation branch
            ResumeKind rkind = yieldPoint.ResumeKind;
            Resume resume = null;
            try {
              result = yieldPoint.CallBranch<B>(this, cont, out resume);
            }
            catch (Exception exn) {
              if (resume.HasResumed || rkind == ResumeKind.Never) throw;
              yieldPoint.RunFinalizers<B>(this, resume, new FinalizeThrowException<B>(this, exn));
            }
            // if we exit without resuming..
            if (!resume.HasResumed && rkind != ResumeKind.Never) {
              if (Op.yielding) {
                // extend the continuation in case we are yielding
                if (Op.yieldResumeKind != ResumeKind.Never) {
                  Op.yieldCont = ((Cont<B>)Op.yieldCont).ComposeRunBranch(this, yieldPoint, resume);
                }
              }
              else {
                // we returned without resuming; run finalizers first
                yieldPoint.RunFinalizers<B>(this, resume, new FinalizeReturnException<B>(this, result));
              }
            }
            tailresumed = (resume.HasResumed && rkind == ResumeKind.Tail);
          }
        }
      }
      while (tailresumed);
      return result;
    }

    public B RunBranch<A>( Cont<A,B> cont, A arg, Exception exnarg, YieldPoint yieldPoint, Resume resume) {
      B result;
      try {
        result = cont.Resume(arg,exnarg);        
      }
      catch(Exception exn) {
        if (resume.HasResumed || yieldPoint.ResumeKind == ResumeKind.Never) throw;
        result = yieldPoint.RunFinalizers<B>(this, resume, new FinalizeThrowException<B>(this, exn));
      }
      // if we exit without resuming
      if (!resume.HasResumed && yieldPoint.ResumeKind != ResumeKind.Never) {
        if (Op.yielding) {
          // extend the continuation in case we are yielding
          if (Op.yieldResumeKind != ResumeKind.Never) {
            Op.yieldCont = ((Cont<B>)Op.yieldCont).ComposeRunBranch(this, yieldPoint, resume);
          }
        }
        else {
          // we returned without resuming; run finalizers first
          // yieldPoint.RunFinalizers<B>(this, resume, new FinalizeReturnException<B>(this, result));
        }
      }
      return result;
    }

    public override R HandleTailBranch<O, R>(Branch ybranch, O op, int skip) {
      // push skip handler so operations in the tail branch get handled correctly
      Handler hskip = Handler.Skip(skip);
      TailResume<R> resume;
      B result;
      try {
        result = CallTailBranch<O, R>((IBranch<O, R, B>)ybranch, op, out resume);
      }
      catch (Exception exn) {
        // Raised exception in the branch
        // we finalize back to the handler and rethrow from there.
        // note: this is done for `FinalizeException`s too.
        throw new FinalizeThrowException<B>(this, exn);
      }
      finally {
        hskip.UnSkip(skip);
      }

      // we are yielding; extend the continuation with the skip frame
      if (Op.yielding) {
        if (Op.yieldResumeKind != ResumeKind.Never) {
          // when the continuation is resumed, it calls `HandleTailResume`
          Op.yieldCont = ((Cont<B>)Op.yieldCont).ComposeTailResume<R,B>(this, resume, skip);
        }
        return default(R);
      }
      else if (!resume.HasResumed) {
        // Returned directly from the branch; 
        // we finalize back to the handler and return from there.
        throw new FinalizeReturnException<B>(this, result);
      }
      else return resume.GetResumeArg(this); 
    }

    public R HandleTailResume<R, A>(Cont<A, B> cont, A arg, Exception exnarg, TailResume<R> resume, int skip) {
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
        throw new FinalizeThrowException<B>(this, exn);
      }
      finally {
        hskip.UnSkip(skip);
      }

      // we are yielding; extend the continuation with the skip frame
      if (Op.yielding) {
        if (Op.yieldResumeKind != ResumeKind.Never) {
          Op.yieldCont = ((Cont<B>)Op.yieldCont).ComposeTailResume<R,B>(this, resume, skip);
        }
        return default(R);
      }
      else if (!resume.HasResumed) {
        // Returned directly from the branch; 
        // we finalize back to the handler and return from there.
        throw new FinalizeReturnException<B>(this, result);
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
    public Handler(string effectTag, Branch[] branches, bool linear = false) : base(effectTag, branches, linear) { }

    public abstract B CallReturnFun(A arg);

    public B Handle(Fun0<A> action) {
      B result;
      Push();
      try {
        // todo: slightly wrong as the operations in the return fun are handled by ourselves too
        result = Op.Bind(action.Call(), (A x) => CallReturnFun(x));
      }
      catch (FinalizeException<B> exn) {
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

    public Handler0(string effectTag, Fun1<A, B> returnFun, Branch<B>[] branches, bool linear = false) : base(effectTag, branches, linear) {
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

    public override B CallTailBranch<O, R>(IBranch<O, R, B> ybranch, O op, out TailResume<R> resume) {
      Branch<O, R, B> branch = (Branch<O, R, B>)ybranch;
      Debug.Assert(branch.ResumeKind == ResumeKind.Tail);
      ResumeTailDirect<R> tresume = ResumeTailDirect<R>.Singleton;
      resume = tresume;
      return branch.Call(tresume, op);
    }

    // Convenience
    public Handler0(string effectTag, Func<A, B> returnFun, Branch<B>[] branches, bool linear = false) : base(effectTag, branches, linear) {
      this.returnFun = new Primitive.FunFunc1<A, B>(returnFun);
    }

    public static Fun1<Fun0<A>, B> Create(string effectTag, Fun1<A, B> returnFun, Branch<B>[] branches, bool linear = false) {
      Handler0<A, B> h = new Handler0<A, B>(effectTag, returnFun, branches, linear);
      return new HandlerFun0(h);
    }

    public static Func<Func<A>, B> Create(string effectTag, Func<A, B> returnFun, Branch<B>[] branches, bool linear = false) {
      Handler0<A, B> h = new Handler0<A, B>(effectTag, returnFun, branches, linear);
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
      
      public override B Call(R arg) {
        throw new Exception("Trying to resume an operation that was marked as never-resuming");
      }
    }

    private sealed class ResumeTailDirect<R> : Resume0<R, B>, TailResume<R>
    {
      private static ResumeTailDirect<R> singleton = new ResumeTailDirect<R>();
      public static ResumeTailDirect<R> Singleton {
        get {
          singleton.resumed = false;
          return singleton;
        }
      }

      private R resumeArg;
      
      public R GetResumeArg(Handler h) {
        R arg = resumeArg;
        resumeArg = default(R);
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

    public Handler1(string effectTag, Fun2<A, S, B> returnFun, Branch1<S, B>[] branches, bool linear = false) : base(effectTag, branches, linear) {
      this.returnFun = returnFun;
      local = default(S);
    }

    private Handler1(string effectTag, Fun2<A, S, B> returnFun, Branch[] branches, S local, bool linear = false) : base(effectTag, branches, linear) {
      this.returnFun = returnFun;
      this.local = local;
    }

    public B Handle(Fun0<A> action, S local0) {
      Handler1<S, A, B> h = new Handler1<S, A, B>(effectTag, returnFun, branches, local0, linear); // copy for identity
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


    public override B CallTailBranch<O, R>(IBranch<O, R, B> ybranch, O op, out TailResume<R> resume) {
      Branch1<S, O, R, B> branch = (Branch1<S, O, R, B>)ybranch;
      Debug.Assert(branch.ResumeKind == ResumeKind.Tail);
      ResumeTailDirect1<R> tresume = ResumeTailDirect1<R>.Singleton;
      resume = tresume;
      return branch.Call(tresume, op, local);
    }

    // Convenience
    public Handler1(string effectTag, Func<A, S, B> returnFun, Branch1<S, B>[] branches, bool linear = false) : base(effectTag, branches, linear) {
      this.returnFun = new Primitive.FunFunc2<A, S, B>(returnFun);
    }

    public static Fun2<S, Fun0<A>, B> Create(string effectTag, Fun2<A, S, B> returnFun, Branch1<S, B>[] branches, bool linear = false) {
      Handler1<S, A, B> h = new Handler1<S, A, B>(effectTag, returnFun, branches, linear);
      return new HandlerFun1(h);
    }

    public static Func<S, Func<A>, B> Create(string effectTag, Func<A, S, B> returnFun, Branch1<S, B>[] branches, bool linear = false) {
      Handler1<S, A, B> h = new Handler1<S, A, B>(effectTag, returnFun, branches, linear);
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
      
      public override B Call(R arg, S local) {
        throw new Exception("Trying to resume an operation that was marked as never-resuming"); // + ((Handler)h).effectTag);
      }
    }

    private sealed class ResumeTailDirect1<R> : Resume1<S, R, B>, TailResume<R>
    {
      // not safe for multi threading, use thread static if needed.
      private static ResumeTailDirect1<R> singleton = new ResumeTailDirect1<R>();
      public static ResumeTailDirect1<R> Singleton {
        get {
          singleton.resumed = false;
          return singleton;
        }
      }

      private R resumeArg = default(R);
      private S local = default(S);
      
      public R GetResumeArg(Handler h) {
        ((Handler1<S, B>)h).SetLocal(local);
        R arg = resumeArg;
        resumeArg = default(R);
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