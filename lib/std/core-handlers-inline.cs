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
    Cont<A> ComposeFinally(Fun0<Unit> onFinal);
    Cont<A> ComposeCatch(Fun1<Exception, A> onExn);
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
    public Cont<B> ComposeFinally(Fun0<Unit> onFinal) {
      return new ContComposeFinally(this, onFinal);
    }
    public Cont<B> ComposeCatch(Fun1<Exception, B> onExn) {
      return new ContComposeCatch(this, onExn);
    }
    public Cont<A, B> ComposeHandler(Handler<B> h) {
      return new ContComposeHandler<A>(this, h);
    }
    public Cont<A, B> ComposeHandler1<S>(Handler1<S, B> h, S local) {
      return new ContComposeHandler1<A, S>(this, h, local);
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

      public ContComposeFinally(Cont<A, B> cont, Fun0<Unit> onFinal) {
        this.cont = cont;
        this.onFinal = onFinal;
      }

      public override B Resume(A arg, Exception exn) {
        return Op.HandleFinally<A, B>(cont, arg, exn, onFinal);
      }
    }

    private sealed class ContComposeCatch : Cont<A, B>
    {
      Cont<A, B> cont;
      Fun1<Exception, B> onExn;

      public ContComposeCatch(Cont<A, B> cont, Fun1<Exception, B> onExn) {
        this.cont = cont;
        this.onExn = onExn;
      }

      public override B Resume(A arg, Exception exn) {
        return Op.HandleCatch<A, B>(cont, arg, exn, onExn);
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
    public abstract bool CallBranch<B>(Handler<B> handler, Cont<B> cont, out B result);
    public abstract Cont<B> HandlerCompose<B>(Handler<B> handler, Cont<B> cont);
    public abstract B RunFinalizers<B>(Handler<B> handler, Cont<B> cont, FinalizeException<B> exn);
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

    public override bool CallBranch<B>(Handler<B> handler, Cont<B> cont, out B result) {
      Debug.Assert(HandledBy(handler));
      return handler.CallBranch<O, R>((IBranch<O, R, B>)branch, (Cont<R, B>)cont, op, out result);
    }

    public override Cont<B> HandlerCompose<B>(Handler<B> handler, Cont<B> cont) {
      Debug.Assert(!HandledBy(handler));
      return handler.ContCompose<R>((Cont<R, B>)cont);
    }

    public override B RunFinalizers<B>(Handler<B> handler, Cont<B> cont, FinalizeException<B> exn) {
      Debug.Assert(HandledBy(handler));
      return handler.RunFinalizers<R>((Cont<R, B>)cont, exn);
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
      int skip;
      Branch branch;
      Handler handler;
      Handler.Find(effectTag, opName, opTag, out handler, out branch, out skip);
      yieldResumeKind = branch.ResumeKind;
      if (yieldResumeKind == ResumeKind.Tail && (tailYieldCount < tailYieldMax || handler.IsLinear)) {
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

    public static R YieldOpX1<O, R, E>(string effectTag, string opName, int opTag, O op) {
      int skip;
      Branch branch;
      Handler handler;
      Handler.Find(effectTag, opName, opTag, out handler, out branch, out skip);
      branch = ((IBranchX1)branch).TypeApply<O,R,E>();
      yieldResumeKind = branch.ResumeKind;
      if (yieldResumeKind == ResumeKind.Tail && (tailYieldCount < tailYieldMax || handler.IsLinear)) {
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
        return HandleFinally<object, A>(new ContAction<A>(() => {
          return HandleCatch<A>(action, onExn);
        }), null, null, onFinal);
      }
    }

    public static A HandleCatch<A>(Fun0<A> action, Fun1<Exception, A> onExn) {
      return HandleCatch(new ContAction<A>(action), null, null, onExn);
    }
    public static A HandleFinally<A>(Fun0<A> action, Fun0<Unit> onFinal) {
      return HandleFinally(new ContAction<A>(action), null, null, onFinal);
    }

    public static A HandleCatch<R, A>(Cont<R, A> cont, R arg, Exception exception, Fun1<Exception, A> onExn) {
      A result;
      try {
        result = cont.Resume(arg, exception);
        if (yielding && yieldResumeKind != ResumeKind.Never) {
          // extend continuation to handle exceptions again on the resume
          yieldCont = ((Cont<A>)yieldCont).ComposeCatch(onExn);
        }
      }
      catch (FinalizeException) {
        // never handle finalize exceptions
        throw;
      }
      catch (Exception exn) {
        result = onExn.Call(exn);
      }
      return result;
    }

    public static A HandleFinally<R, A>(Cont<R, A> cont, R arg, Exception exception, Fun0<Unit> onFinal) {
      A result;
      try {
        result = cont.Resume(arg, exception);
        if (yielding && yieldResumeKind != ResumeKind.Never) {
          // extend continuation, don't finalize on yield! (which is why we cannot use `finally`)
          yieldCont = ((Cont<A>)yieldCont).ComposeFinally(onFinal);
          return result;
        }
      }
      catch (Exception) {
        // finalize and rethrow
        onFinal.Call();
        throw;
      }
      onFinal.Call();
      return result;
    }

  }


  public abstract class FinalizeException : Exception
  {
    private readonly Handler handler;

    public bool HandledBy(Handler h) {
      return (h == handler);
    }

    public FinalizeException(Handler handler) : base("Internal: operation in a linear handler threw an exception or returned directly") {
      this.handler = handler;
    }
  }

  public abstract class FinalizeException<B> : FinalizeException
  {
    public FinalizeException(Handler handler) : base(handler) { }

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
  public interface Resume<in R, in B> : Fun1<R, B>
  {
    bool HasResumed { get; }
  }

  public interface Resume1<in S, in R, in B> : Fun2<R, S, B>
  {
    bool HasResumed { get; }
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
    Fun2<Resume<R, B>, O, B> branchFun;

    public Branch(ResumeKind resumeKind, string opName,
                    Fun2<Fun1<R, B>, O, B> branchFun) : base(resumeKind, opName) {
      this.branchFun = branchFun;
    }

    public B Call(Resume<R, B> resume, O op) {
      return branchFun.Call(resume, op);
    }

    // Convenience: wrap into C# functions
    public Branch(ResumeKind resumeKind, string opName,
                    Func<Resume<R, B>, O, B> branchFun) : base(resumeKind, opName) {
      this.branchFun = new Primitive.FunFunc2<Resume<R, B>, O, B>(branchFun);
    }

    public Branch(ResumeKind resumeKind, string opName, Func<Func<R, B>, O, B> branchFun) : base(resumeKind, opName) {
      this.branchFun = new Primitive.FunFunc2<Resume<R, B>, O, B>((Resume<R, B> r, O op) => {
        return branchFun((arg) => r.Call(arg), op);
      });
    }

  }

  public interface IBranchX1
  {
    Branch TypeApply<O,R,E>();
  }

  public sealed class BranchX1<B> : Branch<B>, IBranchX1
  {
    TypeFun1 branchFun;

    public BranchX1(ResumeKind resumeKind, string opName, TypeFun1 branchFun) : base(resumeKind,opName) {
      this.branchFun = branchFun;
    }

    public Branch TypeApply<O,R,E>() {
      object ofun = branchFun.TypeApply<E>();
      Fun2<Fun1<R, B>, O, B> fun = ofun as Fun2<Fun1<R, B>, O, B>;
      if (fun == null) {
        // convenience; allow a delegate as a resume function
        Func<Resume<R, B>, O, B> ffun = ofun as Func<Resume<R, B>, O, B>;
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

    public B Call(Resume1<S, R, B> resume, O op, S local) {
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
  // Handlers returning a result of type B
  public abstract class Handler<B> : Handler
  {
    public Handler(string effectTag, Branch[] branches, bool linear = false) : base(effectTag, branches, linear) { }

    public abstract bool CallBranch<O, R>(IBranch<O, R, B> branch, Cont<R, B> cont, O op, out B result);
    public abstract bool CallTailBranch<O, R>(IBranch<O, R, B> branch, O op, out B result, out R tailResumeArg);
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
            bool resumed = false;
            try {
              resumed = yieldPoint.CallBranch<B>(this, cont, out result);
            }
            catch (Exception exn) {
              if (resumed || rkind == ResumeKind.Never) throw;  // rethrow, no need for finalization
              // we did not resume yet; run finalizers and rethrow
              result = yieldPoint.RunFinalizers<B>(this, cont, new FinalizeThrowException<B>(this, exn));
            }
            if (!resumed && !Op.yielding && rkind != ResumeKind.Never) {
              // we returned without resuming; run finalizers first
              result = yieldPoint.RunFinalizers<B>(this, cont, new FinalizeReturnException<B>(this, result));
            }
            tailresumed = (resumed && rkind == ResumeKind.Tail);
          }
        }
      }
      while (tailresumed);
      return result;
    }


    public override R HandleTailBranch<O, R>(Branch ybranch, O op, int skip) {
      B result;
      R tailResumeArg;
      bool resumed;

      // push skip handler so operations in the tail branch get handled correctly
      Handler hskip = Handler.Skip(skip);
      try {
        resumed = CallTailBranch<O, R>((IBranch<O, R, B>)ybranch, op, out result, out tailResumeArg);
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

      if (!resumed) {
        // Returned directly from the branch; 
        // we finalize back to the handler and return from there.
        throw new FinalizeReturnException<B>(this, result);
      }
      return tailResumeArg; // TODO: on yielding, extend continuation with skip frame
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

    public override bool CallBranch<O, R>(IBranch<O, R, B> ybranch, Cont<R, B> cont, O op, out B result) {
      Branch<O, R, B> branch = (Branch<O, R, B>)ybranch;
      ResumeKind rkind = branch.ResumeKind;
      if (rkind == ResumeKind.Tail) {
        R tailResumeArg;
        bool resumed = CallTailBranch(branch, op, out result, out tailResumeArg);
        if (resumed) {
          result = ResumeCont(cont, tailResumeArg);
        }
        return resumed;
      }
      else {
        Resume<R, B> resume;
        if (rkind == ResumeKind.Never) resume = ResumeNever<R>.singleton;
        else resume = new ResumeNormal<R>(this, cont);
        result = branch.Call(resume, op);
        return resume.HasResumed;
      }
    }

    public override bool CallTailBranch<O, R>(IBranch<O, R, B> ybranch, O op, out B result, out R tailResumeArg) {
      Branch<O, R, B> branch = (Branch<O, R, B>)ybranch;
      Debug.Assert(branch.ResumeKind == ResumeKind.Tail);
      ResumeTailDirect<R> resume = ResumeTailDirect<R>.singleton;
      result = branch.Call(resume, op);
      return resume.GetResumed(out tailResumeArg);
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

    private sealed class ResumeNormal<R> : Resume<R, B>
    {
      Handler<B> handler;
      Cont<R, B> cont;
      bool resumed = false;

      public ResumeNormal(Handler<B> h, Cont<R, B> c) {
        handler = h;
        cont = c;
      }

      public bool HasResumed {
        get { return resumed; }
      }


      public object Apply(R arg) {
        resumed = true;
        return handler.HandleResume<R>(cont, arg);
      }
    }

    private sealed class ResumeNever<R> : Resume<R, B>
    {
      public static ResumeNever<R> singleton = new ResumeNever<R>();
      public bool HasResumed {
        get { return false; }
      }
      public object Apply(R arg) {
        throw new Exception("Trying to resume an operation that was marked as never-resuming");
      }
    }

    private sealed class ResumeTailDirect<R> : Resume<R, B>
    {
      public static ResumeTailDirect<R> singleton = new ResumeTailDirect<R>();
      private bool resumed = false;
      private R resumeArg;
      public bool HasResumed {
        get { return resumed; }
      }
      public bool GetResumed(out R resumeArg) {
        bool resumed = this.resumed;
        this.resumed = false;
        resumeArg = this.resumeArg;
        this.resumeArg = default(R);
        return resumed;
      }
      public object Apply(R arg) {
        Debug.Assert(!resumed);
        resumed = true;
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
    B HandleResume<R>(Cont<R, B> cont, R arg, S local, Exception exn = null);
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
    public B HandleResume<R>(Cont<R, B> cont, R arg, S newLocal, Exception exn) {
      local = newLocal;
      return HandleResume(cont, arg, exn);
    }

    public override B CallReturnFun(A arg) {
      return returnFun.Call(arg, local);
    }

    public override bool CallBranch<O, R>(IBranch<O, R, B> ybranch, Cont<R, B> cont, O op, out B result) {
      Branch1<S, O, R, B> branch = (Branch1<S, O, R, B>)ybranch;
      ResumeKind rkind = branch.ResumeKind;
      if (rkind == ResumeKind.Tail) {
        R tailResumeArg;
        bool resumed = CallTailBranch(branch, op, out result, out tailResumeArg);
        if (resumed) {
          result = ResumeCont(cont, tailResumeArg);
        }
        return resumed;
      }
      else {
        Resume1<S, R, B> resume;
        if (rkind == ResumeKind.Never) resume = ResumeNever1<R>.singleton;
        else resume = new ResumeNormal1<R>(this, cont);
        result = branch.Call(resume, op, local);
        return resume.HasResumed;
      }
    }


    public override bool CallTailBranch<O, R>(IBranch<O, R, B> ybranch, O op, out B result, out R tailResumeArg) {
      Branch1<S, O, R, B> branch = (Branch1<S, O, R, B>)ybranch;
      Debug.Assert(branch.ResumeKind == ResumeKind.Tail);
      ResumeTailDirect1<R> resume = ResumeTailDirect1<R>.singleton;
      result = branch.Call(resume, op, local);
      return resume.GetResumed(out tailResumeArg, out local);
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
      Handler1<S, B> handler;
      Cont<R, B> cont;
      bool resumed = false;

      public ResumeNormal1(Handler1<S, B> h, Cont<R, B> c) {
        handler = h;
        cont = c;
      }

      public bool HasResumed {
        get { return resumed; }
      }

      public object Apply(R arg, S local) {
        resumed = true;
        return handler.HandleResume(cont, arg, local);
      }
    }

    private sealed class ResumeNever1<R> : Resume1<S, R, B>
    {
      public static ResumeNever1<R> singleton = new ResumeNever1<R>();
      public bool HasResumed {
        get { return false; }
      }
      public object Apply(R arg, S local) {
        throw new Exception("Trying to resume an operation that was marked as never-resuming"); // + ((Handler)h).effectTag);
      }
    }

    private sealed class ResumeTailDirect1<R> : Resume1<S, R, B>
    {
      // not safe for multi threading, use thread static if needed.
      public static ResumeTailDirect1<R> singleton = new ResumeTailDirect1<R>();
      private bool resumed = false;
      private R resumeArg = default(R);
      private S local = default(S);

      public bool HasResumed {
        get { return resumed; }
      }

      public bool GetResumed(out R resumeArg, out S local) {
        bool resumed = this.resumed;
        this.resumed = false;
        local = this.local;
        this.local = default(S);
        resumeArg = this.resumeArg;
        this.resumeArg = default(R);
        return resumed;
      }

      public object Apply(R arg, S local) {
        Debug.Assert(!resumed);
        resumed = true;
        resumeArg = arg;
        this.local = local;
        return default(B);
      }
    }
  }


  #endregion

}