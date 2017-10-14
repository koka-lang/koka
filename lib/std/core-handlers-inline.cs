/*---------------------------------------------------------------------------
  Copyright 2017 Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

namespace Eff
{

  // --------------------------------------------------
  // Continuations
  // --------------------------------------------------
  public interface Cont : Fun1<object, object>
  {
  }


  class ContId : Cont
  {
    public static ContId singleton = new ContId();

    public object Apply(object arg) {
      return arg;
    }
  }

  class BindCompose<A, B> : Cont
  {
    Cont cont;
    Fun1<A, B> bind;

    public BindCompose(Fun1<A, B> bind, Cont cont) {
      this.cont = cont;
      this.bind = bind;
    }

    public object Apply(object arg) {
      return Op.Bind((A)cont.Apply(arg), bind);
    }
  }


  class HandlerCompose0<B> : Cont
  {
    Cont cont;
    Handler0<B> handler;

    public HandlerCompose0(Handler0<B> handler, Cont cont) {
      this.cont = cont;
      this.handler = handler;
    }

    public object Apply(object arg) {
      return handler.Resume(cont, arg);
    }
  }

  class HandlerCompose1<S, B> : Cont
  {
    Cont cont;
    Handler1<S, B> handler;
    S state;

    public HandlerCompose1(Handler1<S, B> handler, Cont cont, S state) {
      this.cont = cont;
      this.handler = handler;
      this.state = state;
    }

    public object Apply(object arg) {
      return handler.Resume(cont, state, arg);
    }
  }


  struct Resume0<A, B> : Fun1<A, B>
  {
    Cont cont;
    Handler0<B> handler;

    public Resume0(Handler0<B> handler, Cont cont) {
      this.cont = cont;
      this.handler = handler;
    }
    public object Apply(A x) {
      return handler.Resume(cont, x);
    }
  }

  struct Resume1<S, A, B> : Fun2<S, A, B>
  {
    Cont cont;
    Handler1<S, B> handler;

    public Resume1(Handler1<S, B> handler, Cont cont) {
      this.cont = cont;
      this.handler = handler;
    }
    public object Apply(S state, A x) {
      return handler.Resume(cont, state, x);
    }
  }


  // --------------------------------------------------
  // Operations
  // --------------------------------------------------

  public abstract class Op
  {
    public static Op Yielding = null;
    public Cont Cont;
    public readonly Handler Handler;
    public readonly OpBranch OpBranch;
    public abstract object OpValue { get; }

    public Op(Handler handler, OpBranch opBranch) {
      Handler = handler;
      OpBranch = opBranch;
      Cont = (opBranch == null || opBranch.ResumesNever ? null : ContId.singleton);
    }

    public bool IsTailCall {
      get { return (this == TailCallOp._tailCallOp); }
    }

    public A MakeTailCall<A>() {
      return TailCallOp._MakeTailCall<A>();
    }

    public static Op TailCall {
      get {
        return (Yielding == TailCallOp._tailCallOp ? Yielding : null);
      }
    }

    private static bool isInBindContext;

    public static bool IsInBindContext() {
      bool result = isInBindContext;
      isInBindContext = false;
      return result;
    }

    public static Unit InBindContext() {
      isInBindContext = true;
      return Unit.unit;
    }

    // --------------------------------------------------
    // Bind
    // --------------------------------------------------

    public static B Bind<A, B>(A x, Fun1<A, B> next) {
      Op op;
      while ((op = Yielding) != null) {
        if (op.IsTailCall) {
          x = op.MakeTailCall<A>();
        }
        else {
          op.Cont = new BindCompose<A, B>(next, op.Cont);
          return default(B);
        }
      }
      return next.Call(x);
    }

    // For convenience
    public static B Bind<A, B>(A x, Func<A, B> next) {
      return Bind(x, new Primitive.FunFunc1<A, B>(next));
    }

#if DEBUG
    public static string yieldingEffectTag;
    public static string yieldingOpTag;
    public static string ShowYieldingOp() {
      if (Yielding == null) return "<none>";
      return yieldingEffectTag + "/" + yieldingOpTag;
    }
#endif
  }

  class TailCallOp : Op
  {
    private static object TailCallFun = null;

    public TailCallOp() : base(null, null) { }
    public override object OpValue { get { return TailCallFun; } }

    public static TailCallOp _tailCallOp = new TailCallOp();

    public static A _MakeTailCall<A>() {
      Yielding = null;
      Func<A> tailCall = (Func<A>)TailCallFun;
      TailCallFun = null;
      return tailCall();
    }
    public static A Yield<A>(Func<A> tailCall) {
      Yielding = _tailCallOp;
      TailCallFun = tailCall;
      return default(A);
    }
  }

  public class Op<O, R> : Op
  {
    private O opValue;
    public override object OpValue { get { return opValue; } }

    public Op(O opValue, Handler handler, OpBranch opBranch) :
      base(handler, opBranch) {
      this.opValue = opValue;
    }

    public static R Yield(string effectTag, string opTag, O opValue) {
#if DEBUG
      // this can happen if you forget "bind"
      if (Yielding != null) throw new Exception("Should not happen: yielding a new operation while yielding!: " + effectTag + "/" + opTag + ", while yielding " + ShowYieldingOp());
#endif
      Handler handler;
      OpBranch opBranch;
      int skipped;
      Handler.FindHandler(effectTag, opTag, out handler, out opBranch, out skipped);
      if (opBranch.IsTailResume) {
        // Optimized tail resumptions become a direct method call
        return TailResume((OpTailBranch<O, R>)opBranch, handler, skipped, opValue);
      }
      else {
        // In all other cases we unwind the stack to the handler
        Op<O, R> op = new Op<O, R>(opValue, handler, opBranch);
        op.Cont = (op.OpBranch.ResumesNever ? null : ContId.singleton);
        Yielding = op;
        return default(R);
      }
    }

    private static R TailResume(OpTailBranch<O, R> opBranch, Handler handler, int skipped, O opValue) {
      SkipHandler s = SkipHandler.Create(skipped + 1);
      s.Push();
      R result = opBranch.TailInvoke(handler, opValue);
      Op op;
      while ((op = TailCall) != null) {
        result = op.MakeTailCall<R>();
      }
      s.Pop();
      return TailResumeBind(result, s);
    }

    private static R TailResumeBind(R result, SkipHandler s) {
      Op op = Yielding;
      if (op != null) {
        Debug.Assert(!op.IsTailCall);
        op.Cont = new SkipCompose(s, op.Cont);
        return default(R);
      }
      return result;
    }

    private class SkipCompose : Cont
    {
      private Cont cont;
      private SkipHandler s;
      public SkipCompose(SkipHandler s, Cont cont) {
        this.cont = cont;
        this.s = s;
      }
      public object Apply(object arg) {
        s.Push();
        R result = (R)cont.Apply(arg);
        Op op;
        while ((op = TailCall) != null) {
          result = op.MakeTailCall<R>();
        }
        s.Pop();
        return TailResumeBind(result, s);
      }
    }
  }

  

  // --------------------------------------------------
  // Op Handlers
  // --------------------------------------------------
  public enum ResumeKind
  {
    Never, Tail, Once, Many
  }

  public abstract class OpBranch
  {
    public readonly string OpTag;
    public readonly ResumeKind ResumeKind;
    public bool IsTailResume { get { return ResumeKind == ResumeKind.Tail; } }
    public bool ResumesNever { get { return ResumeKind == ResumeKind.Never; } }
    public bool ResumesOnce { get { return ResumeKind != ResumeKind.Many; } }

    public OpBranch(ResumeKind resumeKind, string opTag) {
      OpTag = opTag;
      ResumeKind = resumeKind;
    }
  }

  public abstract class OpTailBranch<O, R> : OpBranch
  {
    public OpTailBranch(string opTag) : base(ResumeKind.Tail, opTag) { }
    public abstract R TailInvoke(Handler h, O opValue);
  }

  public interface OpBranch0<B>
  {
    B Invoke(Handler0<B> handler, Cont cont, object opValue);
  }

  public interface OpBranch0<O, B> : OpBranch0<B>
  {
    B Invoke(Handler0<B> handler, Cont cont, O opValue);
  }


  public class OpBranch0<O, R, B> : OpBranch, OpBranch0<O, B>
  {
    public static OpBranch0<O,R,B> Create( ResumeKind resumeKind, string opTag, Func<Fun1<R,B>,O,B> branch ) {
      return Create(resumeKind, opTag, new Primitive.FunFunc2<Fun1<R, B>, O, B>(branch));  
    }
    public static OpBranch0<O, R, B> Create(ResumeKind resumeKind, string opTag, Fun2<Fun1<R, B>, O, B> branch) {
      return new OpBranch0<O, R, B>(resumeKind, opTag, branch);
    }

    private Fun2<Fun1<R, B>, O, B> branch;

    protected OpBranch0(ResumeKind resumeKind, string opTag,
                Fun2<Fun1<R, B>, O, B> branch) : base(resumeKind, opTag) {
      this.branch = branch;
    }

    public OpBranch0(string opTag,
                      Fun2<Fun1<R, B>, O, B> branch) : base(ResumeKind.Many, opTag) {
      this.branch = branch;
    }

    public B Invoke(Handler0<B> handler, Cont cont, object opValue) {
      return Invoke(handler, cont, (O)opValue);
    }
    public B Invoke(Handler0<B> handler, Cont cont, O opValue) {
      return branch.Call(new Resume0<R, B>(handler, cont), opValue);
    }
  }

  public class OpTailBranch0<O, R, B> : OpTailBranch<O, R>, OpBranch0<O, B>
  {
    public static OpTailBranch0<O, R, B> Create(string opTag, Func<O, R> branch) {
      return Create(opTag, new Primitive.FunFunc1<O, R>(branch));
    }
    public static OpTailBranch0<O, R, B> Create(string opTag, Fun1<O,R> branch) {
      return new OpTailBranch0<O, R, B>(opTag, branch);
    }

    private Fun1<O, R> branch;

    public OpTailBranch0(string opTag, Fun1<O, R> branch) : base(opTag) {
      this.branch = branch;
    }

    public B Invoke(Handler0<B> handler, Cont cont, object opValue) {
      return Invoke(handler, cont, (O)opValue);
    }

    public B Invoke(Handler0<B> handler, Cont cont, O opValue) {
      return handler.Resume(cont, TailInvoke(handler, opValue));
    }

    public override R TailInvoke(Handler h, O opValue) {
      return branch.Call(opValue);
    }
  }

  public interface OpBranch1<S, B>
  {
    B Invoke(Handler1<S, B> handler, Cont cont, object opValue, S state);
  }

  public interface OpBranch1<S, O, B> : OpBranch1<S, B>
  {
    B Invoke(Handler1<S, B> handler, Cont cont, O opValue, S state);
  }

  public class OpBranch1<S, O, R, B> : OpBranch, OpBranch1<S, O, B>
  {
    public static OpBranch1<S, O, R, B> Create(ResumeKind resumeKind, string opTag, Func<Fun2<S, R, B>, O, S, B> branch) {
      return Create(resumeKind, opTag, new Primitive.FunFunc3<Fun2<S, R, B>, O, S, B>(branch));
    }
    public static OpBranch1<S, O, R, B> Create(ResumeKind resumeKind, string opTag, Fun3<Fun2<S, R, B>, O, S, B> branch) {
      return new OpBranch1<S, O, R, B>(resumeKind, opTag, branch);
    }

    private Fun3<Fun2<S, R, B>, O, S, B> branch;

    public OpBranch1(ResumeKind resumeKind, string opTag,
                Fun3<Fun2<S, R, B>, O, S, B> branch) : base(resumeKind, opTag) {
      this.branch = branch;
    }

    public B Invoke(Handler1<S, B> handler, Cont cont, object opValue, S state) {
      return Invoke(handler, cont, (O)opValue, state);
    }

    public B Invoke(Handler1<S, B> handler, Cont cont, O opValue, S state) {
      return branch.Call(new Resume1<S, R, B>(handler, cont), opValue, state);
    }
  }

  public class OpTailBranch1<S, O, R, B> : OpTailBranch<O, R>, OpBranch1<S, O, B>
  {
    public static OpTailBranch1<S, O, R, B> Create(string opTag, Func<O, S, std_core._Tuple2_<S,R>> branch) {
      return Create(opTag, new Primitive.FunFunc2<O, S, std_core._Tuple2_<S,R>>(branch));
    }
    public static OpTailBranch1<S, O, R, B> Create(string opTag, Fun2<O, S, std_core._Tuple2_<S,R>> branch) {
      return new OpTailBranch1<S, O, R, B>(opTag, branch);
    }

    private Fun2<O, S, std_core._Tuple2_<S, R>> branch;

    public OpTailBranch1(string opTag, Fun2<O, S, std_core._Tuple2_<S, R>> branch) : base(opTag) {
      this.branch = branch;
    }

    public B Invoke(Handler1<S, B> handler, Cont cont, object opValue, S state) {
      return Invoke(handler, cont, (O)opValue, state);
    }

    public B Invoke(Handler1<S, B> handler, Cont cont, O opValue, S state) {
      std_core._Tuple2_<S, R> res = branch.Call(opValue, state);
      return handler.Resume(cont, res.fst, res.snd);
    }

    public override R TailInvoke(Handler handler, O opValue) {
      return ((Handler1<S, B>)handler).TailInvoke<O, R>(branch, opValue);
    }
  }

  // --------------------------------------------------
  // Handlers
  // --------------------------------------------------

  public abstract class Handler
  {
    public readonly string EffectTag;
    public readonly bool IsShallow;     // ToDo: not yet fully implemented
    public readonly int Skip;

    public Handler(bool isShallow, string effectTag, OpBranch[] opBranches) {
      Skip = 0;
      IsShallow = isShallow;
      EffectTag = effectTag;
      this.opBranches = opBranches;
    }

    public Handler(int skip) {
      Skip = skip;
      IsShallow = false;
      EffectTag = "<skip>";
      opBranches = null;
    }

    private OpBranch[] opBranches;
    private static Stack<Handler> handlers = new Stack<Handler>(100);

    public void Push() {
      if (IsShallow) return;
      handlers.Push(this);
    }

    public void Pop() {
      if (IsShallow) return;
      Handler h = handlers.Pop();
      Debug.Assert(h == this);
    }

    public static void FindHandler(string effectTag, string opTag, out Handler handler, out OpBranch branch, out int skipped) {
      int index = 0;
      int toSkip = 0;
      foreach (Handler h in handlers) {
        index++;
        if (toSkip > 0) {
          // skipping
          toSkip--;
        }
        else if (h.EffectTag == effectTag) {
          // found!
          skipped = index - 1;
          handler = h;
          foreach (OpBranch b in h.opBranches) {
            if (b.OpTag == opTag) {
              branch = b;
              return;
            }
          }
          throw new Exception("Should not happen: operation not handled for effect:" + effectTag + "/" + opTag);
        }
        else {
          // possible skip frames
          toSkip = h.Skip;
        }
      }
      throw new Exception("Should not happen: unhandled operation: " + effectTag + "/" + opTag);
    }
  }

  class SkipHandler : Handler
  {
    public SkipHandler(int skip) : base(skip) { }

    private static SkipHandler[] cache = {
        new SkipHandler(1), new SkipHandler(2), new SkipHandler(3),
        new SkipHandler(4), new SkipHandler(5), new SkipHandler(6)
      };

    public static SkipHandler Create(int skip) {
      if (skip <= 6) {
        return cache[skip - 1];
      }
      else {
        return new SkipHandler(skip);
      }
    }
  }

  public abstract class Handler0<B> : Handler
  {
    public Handler0(bool isShallow, string effectTag, OpBranch[] opHandlers) :
      base(isShallow, effectTag, opHandlers) {
    }
    public abstract B Resume(Cont c, object arg);
  }

  public class Handler0<A, B> : Handler0<B>
  {
    public static Func<Func<A>, B> Create(string effectTag, Func<A, B> ret, OpBranch0<B>[] opBranches) {
      Handler0<A, B> handler = new Handler0<A, B>(effectTag, new Primitive.FunFunc1<A, B>(ret), opBranches);
      return (Func<A> action) => handler.Handle(new Primitive.FunFunc0<A>(action));
    }

    private class FunHandler : Fun1<Fun0<A>, B>
    {
      Handler0<A, B> handler;
      public FunHandler(Handler0<A, B> handler) {
        this.handler = handler;
      }
      public object Apply( Fun0<A> action ) {
        return handler.Handle(action);
      }
    }
    public static Fun1<Fun0<A>, B> Create(string effectTag, Fun1<A, B> ret, OpBranch0<B>[] opBranches) {
      Handler0<A, B> handler = new Handler0<A, B>(effectTag, ret, opBranches);
      return new FunHandler(handler);
    }

    private Fun1<A, B> ret;

    private static OpBranch[] DownCast( OpBranch0<B>[] branches ) {
      OpBranch[] bs = new OpBranch[branches.Length];
      for(int i = 0; i < branches.Length; i++) {
        bs[i] = (OpBranch)branches[i];
      }
      return bs;
    }

    public Handler0(string effectTag, Fun1<A, B> ret, OpBranch0<B>[] opHandlers) :
      base(false, effectTag, DownCast(opHandlers)) {
      this.ret = ret;
    }

    public override B Resume(Cont c, object arg) {
      Push();
      A result = (A)c.Apply(arg);
      Op op;
      while ((op = Op.TailCall) != null) {
        result = op.MakeTailCall<A>();
      }
      Pop();
      return HandleYld(result);
    }

    public B Handle(Fun0<A> action) {
      Push();
      A result = action.Call();
      Op op;
      while ((op = Op.TailCall) != null) {
        result = op.MakeTailCall<A>();
      }
      Pop();
      return HandleYld(result);
    }

    private B HandleYld(A result) {
      Op op = Op.Yielding;
      if (op != null) {
        Debug.Assert(!op.IsTailCall);
        if (op.Handler != this) {
          // Re-yield the yielding operation
          if (!op.OpBranch.ResumesNever) {
            op.Cont = new HandlerCompose0<B>(this, op.Cont);
          }
          return default(B);
        }
        else {
          // Handle the operation
          // No need to optimize tail resumptions; already done in Yield
          Op.Yielding = null;  // stop yielding
          return ((OpBranch0<B>)op.OpBranch).Invoke(this, op.Cont, op.OpValue);
        }
      }
      else {
        // Return regular result
        return ret.Call(result);
      }
    }
  }

  public abstract class Handler1<S, B> : Handler
  {
    private static OpBranch[] DownCast(OpBranch1<S, B>[] branches) {
      OpBranch[] bs = new OpBranch[branches.Length];
      for (int i = 0; i < branches.Length; i++) {
        bs[i] = (OpBranch)branches[i];
      }
      return bs;
    }

    public Handler1(bool isShallow, string effectTag, OpBranch1<S,B>[] opHandlers) :
      base(isShallow, effectTag, DownCast(opHandlers)) {
    }
    public abstract B Resume(Cont cont, S state, object arg);
    public abstract R TailInvoke<O, R>(Fun2<O, S, std_core._Tuple2_<S, R>> branch, O opValue);
  }

  public class Handler1<S, A, B> : Handler1<S, B>
  {
    public static Func<S, Func<A>, B> Create(string effectTag, Func<S, A, B> ret, OpBranch1<S,B>[] opBranches) {
      Handler1<S, A, B> handler = new Handler1<S, A, B>(effectTag, new Primitive.FunFunc2<S, A, B>(ret), opBranches);
      return ((S state, Func<A> action) => handler.Handle(state, new Primitive.FunFunc0<A>(action)));
    }

    private class FunHandler : Fun2<S, Fun0<A>, B>
    {
      Handler1<S, A, B> handler;
      public FunHandler(Handler1<S, A, B> handler) {
        this.handler = handler;
      }
      public object Apply(S state, Fun0<A> action) {
        return handler.Handle(state,action);
      }
    }
    public static Fun2<S, Fun0<A>, B> Create(string effectTag, Fun2<S, A, B> ret, OpBranch1<S, B>[] opBranches) {
      Handler1<S, A, B> handler = new Handler1<S, A, B>(effectTag, ret, opBranches);
      return new FunHandler(handler);
    }

    private Fun2<S, A, B> ret;
    private S TailState;


    public Handler1(string effectTag, Fun2<S, A, B> ret, OpBranch1<S,B>[] opHandlers) :
      base(false, effectTag, opHandlers) {
      this.ret = ret;
    }

    public override R TailInvoke<O, R>(Fun2<O, S, std_core._Tuple2_<S, R>> branch, O opValue) {
      std_core._Tuple2_<S, R> res = branch.Call(opValue, TailState);
      if (Op.Yielding == null) {
        // avoid allocation of the delegate in the common case
        TailState = res.fst;
        return res.snd;
      }
      else {
        // otherwise use a proper bind
        return Op.Bind(res, (std_core._Tuple2_<S, R> r) => {
          TailState = r.fst;
          return r.snd;
        });
      }
    }

    public override B Resume(Cont cont, S state, object arg) {
      Push();
      TailState = state;
      A result = (A)cont.Apply(arg);
      Op op;
      while ((op = Op.TailCall) != null) {
        result = op.MakeTailCall<A>();
      }
      Pop();
      return HandleYld(result, TailState);
    }

    public B Handle(S state, Fun0<A> action) {
      Push();
      TailState = state;
      A result = action.Call();
      Op op;
      while ((op = Op.TailCall) != null) {
        result = op.MakeTailCall<A>();
      }
      Pop();
      return HandleYld(result, TailState);
    }

    private B HandleYld(A result, S state) {
      Op op = Op.Yielding;
      if (op != null) {
        Debug.Assert(!op.IsTailCall);
        if (op.Handler != this) {
          // Re-yield the yielding operation
          if (!op.OpBranch.ResumesNever) {
            op.Cont = new HandlerCompose1<S, B>(this, op.Cont, state);
          }
          return default(B);
        }
        else {
          // Handle the operation
          // No need to optimize tail resumptions; already done in Yield
          Op.Yielding = null;  // stop yielding
          return ((OpBranch1<S, B>)op.OpBranch).Invoke(this, op.Cont, op.OpValue, state);
        }
      }
      else {
        // Return regular result
        return ret.Call(state, result);
      }
    }
  }

}

