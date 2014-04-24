package core.chain;

public abstract class Future<T> {
    public static  <T> Future<T> applyF(F<Void, T> f) {
        Promise<T> p = Promise.<T>apply();
        DispatchQueue.default.asyncF(new P0() {
            @Override
            public void apply() {
                return p.successValue(ERROR: Unknown <l>f\void -> T#G\());
            }
        });
        return p;
    }
    public static  <A, B> Future<Tuple2<A, B>> joinAB(Future<A> a,Future<B> b) {
        return Future.<A, B, Tuple2<A, B>>mapABF(a, b, new F2<A, B, Tuple2<A, B>>() {
            @Override
            public Tuple2<A, B> apply(A _a,B _b) {
                return ERROR: Unknown (<l>_a\§A#G§\, <l>_b\§B#G§\);
            }
        });
    }
    public static  <A, B, C> Future<Tuple3<A, B, C>> joinABC(Future<A> a,Future<B> b,Future<C> c) {
        return Future.<A, B, C, Tuple3<A, B, C>>mapABCF(a, b, c, new F3<A, B, C, Tuple3<A, B, C>>() {
            @Override
            public Tuple3<A, B, C> apply(A _a,B _b,C _c) {
                return ERROR: Unknown (<l>_a\§A#G§\, <l>_b\§B#G§\, <l>_c\§C#G§\);
            }
        });
    }
    public static  <A, B, C, D> Future<Tuple4<A, B, C, D>> joinABCD(Future<A> a,Future<B> b,Future<C> c,Future<D> d) {
        return Future.<A, B, C, D, Tuple4<A, B, C, D>>mapABCDF(a, b, c, d, new F4<A, B, C, D, Tuple4<A, B, C, D>>() {
            @Override
            public Tuple4<A, B, C, D> apply(A _a,B _b,C _c,D _d) {
                return ERROR: Unknown (<l>_a\§A#G§\, <l>_b\§B#G§\, <l>_c\§C#G§\, <l>_d\§D#G§\);
            }
        });
    }
    public static  <A, B, C, D, E> Future<Tuple5<A, B, C, D, E>> joinABCDE(Future<A> a,Future<B> b,Future<C> c,Future<D> d,Future<E> e) {
        return Future.<A, B, C, D, E, Tuple5<A, B, C, D, E>>mapABCDEF(a, b, c, d, e, new F5<A, B, C, D, E, Tuple5<A, B, C, D, E>>() {
            @Override
            public Tuple5<A, B, C, D, E> apply(A _a,B _b,C _c,D _d,E _e) {
                return ERROR: Unknown (<l>_a\§A#G§\, <l>_b\§B#G§\, <l>_c\§C#G§\, <l>_d\§D#G§\, <l>_e\§E#G§\);
            }
        });
    }
    public static  <A, B, R> Future<R> mapABF(Future<A> a,Future<B> b,F2<A, B, R> f) {
        Promise<R> p = Promise.<R>apply();
        A _a = null;
        B _b = null;
        AtomicInt n = new AtomicInt();
        a.onCompleteF(new P<Try<A>>() {
            @Override
            public void apply(Try<A> t) {
                if(t.isSuccess()) {
                    _a = t.get();
                    Memory.memoryBarrier();
                    if(n.incrementAndGet().equals(2)) {
                        p.successValue(f.apply(_a, _b));
                    }
                } else {
                    p.completeValue(((Try<R>)t));
                }
            }
        });
        b.onCompleteF(new P<Try<B>>() {
            @Override
            public void apply(Try<B> t) {
                if(t.isSuccess()) {
                    _b = t.get();
                    Memory.memoryBarrier();
                    if(n.incrementAndGet().equals(2)) {
                        p.successValue(f.apply(_a, _b));
                    }
                } else {
                    p.completeValue(((Try<R>)t));
                }
            }
        });
        return p;
    }
    public static  <A, B, C, R> Future<R> mapABCF(Future<A> a,Future<B> b,Future<C> c,F3<A, B, C, R> f) {
        Promise<R> p = Promise.<R>apply();
        A _a = null;
        B _b = null;
        C _c = null;
        AtomicInt n = new AtomicInt();
        a.onCompleteF(new P<Try<A>>() {
            @Override
            public void apply(Try<A> t) {
                if(t.isSuccess()) {
                    _a = t.get();
                    Memory.memoryBarrier();
                    if(n.incrementAndGet().equals(3)) {
                        p.successValue(f.apply(_a, _b, _c));
                    }
                } else {
                    p.completeValue(((Try<R>)t));
                }
            }
        });
        b.onCompleteF(new P<Try<B>>() {
            @Override
            public void apply(Try<B> t) {
                if(t.isSuccess()) {
                    _b = t.get();
                    Memory.memoryBarrier();
                    if(n.incrementAndGet().equals(3)) {
                        p.successValue(f.apply(_a, _b, _c));
                    }
                } else {
                    p.completeValue(((Try<R>)t));
                }
            }
        });
        c.onCompleteF(new P<Try<C>>() {
            @Override
            public void apply(Try<C> t) {
                if(t.isSuccess()) {
                    _c = t.get();
                    Memory.memoryBarrier();
                    if(n.incrementAndGet().equals(3)) {
                        p.successValue(f.apply(_a, _b, _c));
                    }
                } else {
                    p.completeValue(((Try<R>)t));
                }
            }
        });
        return p;
    }
    public static  <A, B, C, D, R> Future<R> mapABCDF(Future<A> a,Future<B> b,Future<C> c,Future<D> d,F4<A, B, C, D, R> f) {
        Promise<R> p = Promise.<R>apply();
        A _a = null;
        B _b = null;
        C _c = null;
        D _d = null;
        AtomicInt n = new AtomicInt();
        a.onCompleteF(new P<Try<A>>() {
            @Override
            public void apply(Try<A> t) {
                if(t.isSuccess()) {
                    _a = t.get();
                    Memory.memoryBarrier();
                    if(n.incrementAndGet().equals(4)) {
                        p.successValue(f.apply(_a, _b, _c, _d));
                    }
                } else {
                    p.completeValue(((Try<R>)t));
                }
            }
        });
        b.onCompleteF(new P<Try<B>>() {
            @Override
            public void apply(Try<B> t) {
                if(t.isSuccess()) {
                    _b = t.get();
                    Memory.memoryBarrier();
                    if(n.incrementAndGet().equals(4)) {
                        p.successValue(f.apply(_a, _b, _c, _d));
                    }
                } else {
                    p.completeValue(((Try<R>)t));
                }
            }
        });
        c.onCompleteF(new P<Try<C>>() {
            @Override
            public void apply(Try<C> t) {
                if(t.isSuccess()) {
                    _c = t.get();
                    Memory.memoryBarrier();
                    if(n.incrementAndGet().equals(4)) {
                        p.successValue(f.apply(_a, _b, _c, _d));
                    }
                } else {
                    p.completeValue(((Try<R>)t));
                }
            }
        });
        d.onCompleteF(new P<Try<D>>() {
            @Override
            public void apply(Try<D> t) {
                if(t.isSuccess()) {
                    _d = t.get();
                    Memory.memoryBarrier();
                    if(n.incrementAndGet().equals(4)) {
                        p.successValue(f.apply(_a, _b, _c, _d));
                    }
                } else {
                    p.completeValue(((Try<R>)t));
                }
            }
        });
        return p;
    }
    public static  <A, B, C, D, E, R> Future<R> mapABCDEF(Future<A> a,Future<B> b,Future<C> c,Future<D> d,Future<E> e,F5<A, B, C, D, E, R> f) {
        Promise<R> p = Promise.<R>apply();
        A _a = null;
        B _b = null;
        C _c = null;
        D _d = null;
        D _e = null;
        AtomicInt n = new AtomicInt();
        a.onCompleteF(new P<Try<A>>() {
            @Override
            public void apply(Try<A> t) {
                if(t.isSuccess()) {
                    _a = t.get();
                    Memory.memoryBarrier();
                    if(n.incrementAndGet().equals(5)) {
                        p.successValue(f.apply(_a, _b, _c, _d, _e));
                    }
                } else {
                    p.completeValue(((Try<R>)t));
                }
            }
        });
        b.onCompleteF(new P<Try<B>>() {
            @Override
            public void apply(Try<B> t) {
                if(t.isSuccess()) {
                    _b = t.get();
                    Memory.memoryBarrier();
                    if(n.incrementAndGet().equals(5)) {
                        p.successValue(f.apply(_a, _b, _c, _d, _e));
                    }
                } else {
                    p.completeValue(((Try<R>)t));
                }
            }
        });
        c.onCompleteF(new P<Try<C>>() {
            @Override
            public void apply(Try<C> t) {
                if(t.isSuccess()) {
                    _c = t.get();
                    Memory.memoryBarrier();
                    if(n.incrementAndGet().equals(5)) {
                        p.successValue(f.apply(_a, _b, _c, _d, _e));
                    }
                } else {
                    p.completeValue(((Try<R>)t));
                }
            }
        });
        d.onCompleteF(new P<Try<D>>() {
            @Override
            public void apply(Try<D> t) {
                if(t.isSuccess()) {
                    _d = t.get();
                    Memory.memoryBarrier();
                    if(n.incrementAndGet().equals(5)) {
                        p.successValue(f.apply(_a, _b, _c, _d, _e));
                    }
                } else {
                    p.completeValue(((Try<R>)t));
                }
            }
        });
        e.onCompleteF(new P<Try<E>>() {
            @Override
            public void apply(Try<E> t) {
                if(t.isSuccess()) {
                    _e = t.get();
                    Memory.memoryBarrier();
                    if(n.incrementAndGet().equals(5)) {
                        p.successValue(f.apply(_a, _b, _c, _d, _e));
                    }
                } else {
                    p.completeValue(((Try<R>)t));
                }
            }
        });
        return p;
    }
    public static  <T> Future<T> successfulResult(T result) {
        return new KeptPromise<T>(new Success<T>(result));
    }
    public abstract Try<T> result();
    public boolean isCompleted() {
        return this.result() != null;
    }
    public boolean isSucceeded() {
        Try<T> __tmp = this.result();
        if(__tmp != null) {
            return this.result().isSuccess();
        } else {
            return false;
        }
    }
    public boolean isFailed() {
        Try<T> __tmp = this.result();
        if(__tmp != null) {
            return this.result().isFailure();
        } else {
            return true;
        }
    }
    public abstract void onCompleteF(P<Try<T>> f);
    public void onSuccessF(P<T> f) {
        onCompleteF(new P<Try<T>>() {
            @Override
            public void apply(Try<T> t) {
                if(t.isSuccess()) {
                    f.apply(t.get());
                }
            }
        });
    }
    public void onFailureF(P<Object> f) {
        onCompleteF(new P<Try<T>>() {
            @Override
            public void apply(Try<T> t) {
                if(t.isFailure()) {
                    f.apply(t.reason());
                }
            }
        });
    }
    public  <R> Future<R> mapF(F<T, R> f) {
        Promise<R> p = Promise.<R>apply();
        onCompleteF(new P<Try<T>>() {
            @Override
            public void apply(Try<T> tr) {
                p.completeValue(tr.mapF<R>(f));
            }
        });
        return p;
    }
    public Future<Void> forF(P<T> f) {
        Promise<Void> p = Promise.<Void>apply();
        onCompleteF(new P<Try<T>>() {
            @Override
            public void apply(Try<T> tr) {
                if(tr.isSuccess()) {
                    f.apply(tr.get());
                    p.successValue(null);
                } else {
                    p.completeValue(((Try<Void>)tr));
                }
            }
        });
        return p;
    }
    public  <R> Future<R> flatMapF(F<T, Future<R>> f) {
        Promise<R> p = Promise.<R>apply();
        onCompleteF(new P<Try<T>>() {
            @Override
            public void apply(Try<T> tr) {
                if(tr.isFailure()) {
                    p.completeValue(((Try<R>)tr));
                } else {
                    Future<R> fut = f.apply(tr.get());
                    fut.onCompleteF(new P<Try<R>>() {
                        @Override
                        public void apply(Try<R> ftr) {
                            p.completeValue(ftr);
                        }
                    });
                }
            }
        });
        return p;
    }
    public Try<T> waitResultPeriod(float period) {
        ConditionLock lock = new ConditionLock(0);
        onCompleteF(new P<Try<T>>() {
            @Override
            public void apply(Try<T> _) {
                lock.lock();
                lock.unlockWithCondition(1);
            }
        });
        if(lock.lockWhenConditionPeriod(1, period)) {
            lock.unlock();
        }
        return this.result();
    }
    public Try<T> waitResult() {
        ConditionLock lock = new ConditionLock(0);
        onCompleteF(new P<Try<T>>() {
            @Override
            public void apply(Try<T> _) {
                lock.lock();
                lock.unlockWithCondition(1);
            }
        });
        lock.lockWhenCondition(1);
        lock.unlock();
        return ERROR: Unknown {
    local __tmp_4 : (^Try#C<§T#G§>)? = <Future#C<T#G>>self.<dIa>result\(^Try#C<§T#G§>)?\
    if((<l>__tmp_4\(^Try#C<§T#G§>)?\ == none<^Try#C<§T#G§>>)) throw "Not null"
else <l>__tmp_4\(^Try#C<§T#G§>)?\
};
    }
    public void waitAndOnSuccessAwaitF(float await,P<T> f) {
        {
            Try<T> __tr = waitResultPeriod(await);
            if(__tr != null) {
                if(__tr.isSuccess()) {
                    f.apply(__tr.get());
                }
            }
        }
    }
    public  <I> void waitAndOnSuccessFlatAwaitF(float await,P<I> f) {
        {
            {
                Try<T> __inline__0___tr = waitResultPeriod(await);
                if(__inline__0___tr != null) {
                    if(__inline__0___tr.isSuccess()) {
                        {
                            T __tr2 = __inline__0___tr.get();
                            ((Traversable<I>)__tr2).forEach(f);
                        }
                    }
                }
            }
        }
    }
    public T getResultAwait(float await) {
        return ERROR: Unknown {
    local __tmp : §(T#G)?§ = <Future#C<T#G>>self.<dI>waitResult(period = <l>await\float\)\(^Try#C<§T#G§>)?\?.<dIa>get\§T#G§\
    if((<l>__tmp\§(T#G)?§\ == none<T#G>)) throw "Not null"
else <l>__tmp\§(T#G)?§\
};
    }
    public  <R> Future<Tuple2<T, R>> joinAnother(Future<R> another) {
        Promise<Tuple2<T, R>> p = Promise.<Tuple2<T, R>>apply();
        T a = null;
        R b = null;
        AtomicInt n = new AtomicInt();
        onCompleteF(new P<Try<T>>() {
            @Override
            public void apply(Try<T> t) {
                if(t.isSuccess()) {
                    a = t.get();
                    Memory.memoryBarrier();
                    if(n.incrementAndGet().equals(2)) {
                        p.successValue(new Tuple<T, R>(a, b));
                    }
                } else {
                    p.completeValue(((Try<Tuple2<T, R>>)t));
                }
            }
        });
        another.onCompleteF(new P<Try<R>>() {
            @Override
            public void apply(Try<R> t) {
                if(t.isSuccess()) {
                    b = t.get();
                    Memory.memoryBarrier();
                    if(n.incrementAndGet().equals(2)) {
                        p.successValue(new Tuple<T, R>(a, b));
                    }
                } else {
                    p.completeValue(((Try<Tuple2<T, R>>)t));
                }
            }
        });
        return p;
    }
    public Future() {
    }
}