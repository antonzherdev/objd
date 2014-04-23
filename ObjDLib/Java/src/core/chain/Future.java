package core.chain;

public abstract class Future<T> {
    public static Future<T> applyF(F<Void, T> f) {
        Promise<T> p = Promise().apply<T>();
        DispatchQueue().default.asyncF(new P0() {
            @Override
            public void apply() {
                return p.successValue(ERROR: Unknown <l>f\void -> T#G\());
            }
        });
        return p;
    }
    public static Future<Tuple2<A, B>> joinAB(Future<A> a,Future<B> b) {
        return Future().mapABF<A, B, Tuple2<A, B>>(a, b, new F2<A, B, Tuple2<A, B>>() {
            @Override
            public Tuple2<A, B> apply(A _a,B _b) {
                return ERROR: Unknown (<l>_a\§A#G§\, <l>_b\§B#G§\);
            }
        });
    }
    public static Future<Tuple3<A, B, C>> joinABC(Future<A> a,Future<B> b,Future<C> c) {
        return Future().mapABCF<A, B, C, Tuple3<A, B, C>>(a, b, c, new F3<A, B, C, Tuple3<A, B, C>>() {
            @Override
            public Tuple3<A, B, C> apply(A _a,B _b,C _c) {
                return ERROR: Unknown (<l>_a\§A#G§\, <l>_b\§B#G§\, <l>_c\§C#G§\);
            }
        });
    }
    public static Future<Tuple4<A, B, C, D>> joinABCD(Future<A> a,Future<B> b,Future<C> c,Future<D> d) {
        return Future().mapABCDF<A, B, C, D, Tuple4<A, B, C, D>>(a, b, c, d, new F4<A, B, C, D, Tuple4<A, B, C, D>>() {
            @Override
            public Tuple4<A, B, C, D> apply(A _a,B _b,C _c,D _d) {
                return ERROR: Unknown (<l>_a\§A#G§\, <l>_b\§B#G§\, <l>_c\§C#G§\, <l>_d\§D#G§\);
            }
        });
    }
    public static Future<Tuple5<A, B, C, D, E>> joinABCDE(Future<A> a,Future<B> b,Future<C> c,Future<D> d,Future<E> e) {
        return Future().mapABCDEF<A, B, C, D, E, Tuple5<A, B, C, D, E>>(a, b, c, d, e, new F5<A, B, C, D, E, Tuple5<A, B, C, D, E>>() {
            @Override
            public Tuple5<A, B, C, D, E> apply(A _a,B _b,C _c,D _d,E _e) {
                return ERROR: Unknown (<l>_a\§A#G§\, <l>_b\§B#G§\, <l>_c\§C#G§\, <l>_d\§D#G§\, <l>_e\§E#G§\);
            }
        });
    }
    public static Future<R> mapABF(Future<A> a,Future<B> b,F2<A, B, R> f) {
        Promise<R> p = Promise().apply<R>();
        A _a = null;
        B _b = null;
        AtomicInt n = new AtomicInt();
        a.onCompleteF(new P<Try<A>>() {
            @Override
            public void apply(Try<A> t) {
                if(t.isSuccess()) {
                    _a = t.get();
                    Memory().memoryBarrier();
                    if(n.incrementAndGet().equals(ERROR: Unknown 2)) {
                        p.successValue(f.apply(_a, _b));
                    }
                } else {
                    p.completeValue(t.ERROR: Unknown cast<Try#C<R#G>>);
                }
            }
        });
        b.onCompleteF(new P<Try<B>>() {
            @Override
            public void apply(Try<B> t) {
                if(t.isSuccess()) {
                    _b = t.get();
                    Memory().memoryBarrier();
                    if(n.incrementAndGet().equals(ERROR: Unknown 2)) {
                        p.successValue(f.apply(_a, _b));
                    }
                } else {
                    p.completeValue(t.ERROR: Unknown cast<Try#C<R#G>>);
                }
            }
        });
        return p;
    }
    public static Future<R> mapABCF(Future<A> a,Future<B> b,Future<C> c,F3<A, B, C, R> f) {
        Promise<R> p = Promise().apply<R>();
        A _a = null;
        B _b = null;
        C _c = null;
        AtomicInt n = new AtomicInt();
        a.onCompleteF(new P<Try<A>>() {
            @Override
            public void apply(Try<A> t) {
                if(t.isSuccess()) {
                    _a = t.get();
                    Memory().memoryBarrier();
                    if(n.incrementAndGet().equals(ERROR: Unknown 3)) {
                        p.successValue(f.apply(_a, _b, _c));
                    }
                } else {
                    p.completeValue(t.ERROR: Unknown cast<Try#C<R#G>>);
                }
            }
        });
        b.onCompleteF(new P<Try<B>>() {
            @Override
            public void apply(Try<B> t) {
                if(t.isSuccess()) {
                    _b = t.get();
                    Memory().memoryBarrier();
                    if(n.incrementAndGet().equals(ERROR: Unknown 3)) {
                        p.successValue(f.apply(_a, _b, _c));
                    }
                } else {
                    p.completeValue(t.ERROR: Unknown cast<Try#C<R#G>>);
                }
            }
        });
        c.onCompleteF(new P<Try<C>>() {
            @Override
            public void apply(Try<C> t) {
                if(t.isSuccess()) {
                    _c = t.get();
                    Memory().memoryBarrier();
                    if(n.incrementAndGet().equals(ERROR: Unknown 3)) {
                        p.successValue(f.apply(_a, _b, _c));
                    }
                } else {
                    p.completeValue(t.ERROR: Unknown cast<Try#C<R#G>>);
                }
            }
        });
        return p;
    }
    public static Future<R> mapABCDF(Future<A> a,Future<B> b,Future<C> c,Future<D> d,F4<A, B, C, D, R> f) {
        Promise<R> p = Promise().apply<R>();
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
                    Memory().memoryBarrier();
                    if(n.incrementAndGet().equals(ERROR: Unknown 4)) {
                        p.successValue(f.apply(_a, _b, _c, _d));
                    }
                } else {
                    p.completeValue(t.ERROR: Unknown cast<Try#C<R#G>>);
                }
            }
        });
        b.onCompleteF(new P<Try<B>>() {
            @Override
            public void apply(Try<B> t) {
                if(t.isSuccess()) {
                    _b = t.get();
                    Memory().memoryBarrier();
                    if(n.incrementAndGet().equals(ERROR: Unknown 4)) {
                        p.successValue(f.apply(_a, _b, _c, _d));
                    }
                } else {
                    p.completeValue(t.ERROR: Unknown cast<Try#C<R#G>>);
                }
            }
        });
        c.onCompleteF(new P<Try<C>>() {
            @Override
            public void apply(Try<C> t) {
                if(t.isSuccess()) {
                    _c = t.get();
                    Memory().memoryBarrier();
                    if(n.incrementAndGet().equals(ERROR: Unknown 4)) {
                        p.successValue(f.apply(_a, _b, _c, _d));
                    }
                } else {
                    p.completeValue(t.ERROR: Unknown cast<Try#C<R#G>>);
                }
            }
        });
        d.onCompleteF(new P<Try<D>>() {
            @Override
            public void apply(Try<D> t) {
                if(t.isSuccess()) {
                    _d = t.get();
                    Memory().memoryBarrier();
                    if(n.incrementAndGet().equals(ERROR: Unknown 4)) {
                        p.successValue(f.apply(_a, _b, _c, _d));
                    }
                } else {
                    p.completeValue(t.ERROR: Unknown cast<Try#C<R#G>>);
                }
            }
        });
        return p;
    }
    public static Future<R> mapABCDEF(Future<A> a,Future<B> b,Future<C> c,Future<D> d,Future<E> e,F5<A, B, C, D, E, R> f) {
        Promise<R> p = Promise().apply<R>();
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
                    Memory().memoryBarrier();
                    if(n.incrementAndGet().equals(ERROR: Unknown 5)) {
                        p.successValue(f.apply(_a, _b, _c, _d, _e));
                    }
                } else {
                    p.completeValue(t.ERROR: Unknown cast<Try#C<R#G>>);
                }
            }
        });
        b.onCompleteF(new P<Try<B>>() {
            @Override
            public void apply(Try<B> t) {
                if(t.isSuccess()) {
                    _b = t.get();
                    Memory().memoryBarrier();
                    if(n.incrementAndGet().equals(ERROR: Unknown 5)) {
                        p.successValue(f.apply(_a, _b, _c, _d, _e));
                    }
                } else {
                    p.completeValue(t.ERROR: Unknown cast<Try#C<R#G>>);
                }
            }
        });
        c.onCompleteF(new P<Try<C>>() {
            @Override
            public void apply(Try<C> t) {
                if(t.isSuccess()) {
                    _c = t.get();
                    Memory().memoryBarrier();
                    if(n.incrementAndGet().equals(ERROR: Unknown 5)) {
                        p.successValue(f.apply(_a, _b, _c, _d, _e));
                    }
                } else {
                    p.completeValue(t.ERROR: Unknown cast<Try#C<R#G>>);
                }
            }
        });
        d.onCompleteF(new P<Try<D>>() {
            @Override
            public void apply(Try<D> t) {
                if(t.isSuccess()) {
                    _d = t.get();
                    Memory().memoryBarrier();
                    if(n.incrementAndGet().equals(ERROR: Unknown 5)) {
                        p.successValue(f.apply(_a, _b, _c, _d, _e));
                    }
                } else {
                    p.completeValue(t.ERROR: Unknown cast<Try#C<R#G>>);
                }
            }
        });
        e.onCompleteF(new P<Try<E>>() {
            @Override
            public void apply(Try<E> t) {
                if(t.isSuccess()) {
                    _e = t.get();
                    Memory().memoryBarrier();
                    if(n.incrementAndGet().equals(ERROR: Unknown 5)) {
                        p.successValue(f.apply(_a, _b, _c, _d, _e));
                    }
                } else {
                    p.completeValue(t.ERROR: Unknown cast<Try#C<R#G>>);
                }
            }
        });
        return p;
    }
    public static Future<T> successfulResult(T result) {
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
            return ERROR: Unknown False;
        }
    }
    public boolean isFailed() {
        Try<T> __tmp = this.result();
        if(__tmp != null) {
            return this.result().isFailure();
        } else {
            return ERROR: Unknown True;
        }
    }
    public abstract void onCompleteF(P<Try<T>> f);
    public Future<R> mapF(F<T, R> f) {
        Promise<R> p = Promise().apply<R>();
        onCompleteF(new P<Try<T>>() {
            @Override
            public void apply(Try<T> tr) {
                p.completeValue(tr.mapF<R>(f));
            }
        });
        return p;
    }
    public Future<Void> forF(P<T> f) {
        Promise<Void> p = Promise().apply<Void>();
        onCompleteF(new P<Try<T>>() {
            @Override
            public void apply(Try<T> tr) {
                if(tr.isSuccess()) {
                    f.apply(tr.get());
                    p.successValue(null);
                } else {
                    p.completeValue(tr.ERROR: Unknown cast<Try#C<^void>>);
                }
            }
        });
        return p;
    }
    public Future<R> flatMapF(F<T, Future<R>> f) {
        Promise<R> p = Promise().apply<R>();
        onCompleteF(new P<Try<T>>() {
            @Override
            public void apply(Try<T> tr) {
                if(tr.isFailure()) {
                    p.completeValue(tr.ERROR: Unknown cast<Try#C<R#G>>);
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
        ConditionLock lock = new ConditionLock(ERROR: Unknown 0);
        onCompleteF(new P<Try<T>>() {
            @Override
            public void apply(Try<T> _) {
                lock.lock();
                lock.unlockWithCondition(ERROR: Unknown 1);
            }
        });
        if(lock.lockWhenConditionPeriod(ERROR: Unknown 1, period)) {
            lock.unlock();
        }
        return this.result();
    }
    public Try<T> waitResult() {
        ConditionLock lock = new ConditionLock(ERROR: Unknown 0);
        onCompleteF(new P<Try<T>>() {
            @Override
            public void apply(Try<T> _) {
                lock.lock();
                lock.unlockWithCondition(ERROR: Unknown 1);
            }
        });
        lock.lockWhenCondition(ERROR: Unknown 1);
        lock.unlock();
        return ERROR: Unknown {
    local __tmp_4 : (^Try#C<§T#G§>)? = <Future#C<T#G>>self.<dIa>result\(^Try#C<§T#G§>)?\
    if((<l>__tmp_4\(^Try#C<§T#G§>)?\ == none<^Try#C<§T#G§>>)) throw "Not null"
else <l>__tmp_4\(^Try#C<§T#G§>)?\
};
    }
    public T getResultAwait(float await) {
        return ERROR: Unknown {
    local __tmp : §(T#G)?§ = <Future#C<T#G>>self.<dI>waitResult(period = <l>await\float\)\(^Try#C<§T#G§>)?\?.<dIa>get\§T#G§\
    if((<l>__tmp\§(T#G)?§\ == none<T#G>)) throw "Not null"
else <l>__tmp\§(T#G)?§\
};
    }
    public Future<Tuple2<T, R>> joinAnother(Future<R> another) {
        Promise<Tuple2<T, R>> p = Promise().apply<Tuple2<T, R>>();
        T a = null;
        R b = null;
        AtomicInt n = new AtomicInt();
        onCompleteF(new P<Try<T>>() {
            @Override
            public void apply(Try<T> t) {
                if(t.isSuccess()) {
                    a = t.get();
                    Memory().memoryBarrier();
                    if(n.incrementAndGet().equals(ERROR: Unknown 2)) {
                        p.successValue(new Tuple<T, R>(a, b));
                    }
                } else {
                    p.completeValue(t.ERROR: Unknown cast<Try#C<^(T#G, R#G)>>);
                }
            }
        });
        another.onCompleteF(new P<Try<R>>() {
            @Override
            public void apply(Try<R> t) {
                if(t.isSuccess()) {
                    b = t.get();
                    Memory().memoryBarrier();
                    if(n.incrementAndGet().equals(ERROR: Unknown 2)) {
                        p.successValue(new Tuple<T, R>(a, b));
                    }
                } else {
                    p.completeValue(t.ERROR: Unknown cast<Try#C<^(T#G, R#G)>>);
                }
            }
        });
        return p;
    }
    public Future() {
    }
}