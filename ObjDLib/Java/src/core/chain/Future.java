package core.chain;

public class Future<T> {
    public static Future<T> applyF(F<Void, T> f) {
        ERROR: Unknown local p : Promise#C<§T#G§> = <to>Promise\Promise#C.class\.<dIt>apply\Promise#C<§T#G§>\;
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
        ERROR: Unknown local p : Promise#C<§R#G§> = <to>Promise\Promise#C.class\.<dIt>apply\Promise#C<§R#G§>\;
        ERROR: Unknown local var _a : A#G = nil;
        ERROR: Unknown local var _b : B#G = nil;
        ERROR: Unknown local n : AtomicInt#C = <to>AtomicInt\AtomicInt#C.class\.<tcI>apply\AtomicInt#C\;
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
        ERROR: Unknown local p : Promise#C<§R#G§> = <to>Promise\Promise#C.class\.<dIt>apply\Promise#C<§R#G§>\;
        ERROR: Unknown local var _a : A#G = nil;
        ERROR: Unknown local var _b : B#G = nil;
        ERROR: Unknown local var _c : C#G = nil;
        ERROR: Unknown local n : AtomicInt#C = <to>AtomicInt\AtomicInt#C.class\.<tcI>apply\AtomicInt#C\;
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
        ERROR: Unknown local p : Promise#C<§R#G§> = <to>Promise\Promise#C.class\.<dIt>apply\Promise#C<§R#G§>\;
        ERROR: Unknown local var _a : A#G = nil;
        ERROR: Unknown local var _b : B#G = nil;
        ERROR: Unknown local var _c : C#G = nil;
        ERROR: Unknown local var _d : D#G = nil;
        ERROR: Unknown local n : AtomicInt#C = <to>AtomicInt\AtomicInt#C.class\.<tcI>apply\AtomicInt#C\;
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
        ERROR: Unknown local p : Promise#C<§R#G§> = <to>Promise\Promise#C.class\.<dIt>apply\Promise#C<§R#G§>\;
        ERROR: Unknown local var _a : A#G = nil;
        ERROR: Unknown local var _b : B#G = nil;
        ERROR: Unknown local var _c : C#G = nil;
        ERROR: Unknown local var _d : D#G = nil;
        ERROR: Unknown local var _e : D#G = nil;
        ERROR: Unknown local n : AtomicInt#C = <to>AtomicInt\AtomicInt#C.class\.<tcI>apply\AtomicInt#C\;
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
        ERROR: Unknown local __tmp : (^Try#C<§T#G§>)? = <Future#C<T#G>>self.<dIa>result\(^Try#C<§T#G§>)?\;
        if(__tmp != null) {
            return this.result().isSuccess();
        } else {
            return ERROR: Unknown False;
        }
    }
    public boolean isFailed() {
        ERROR: Unknown local __tmp : (^Try#C<§T#G§>)? = <Future#C<T#G>>self.<dIa>result\(^Try#C<§T#G§>)?\;
        if(__tmp != null) {
            return this.result().isFailure();
        } else {
            return ERROR: Unknown True;
        }
    }
    public abstract void onCompleteF(P<Try<T>> f);
    public Future<R> mapF(F<T, R> f) {
        ERROR: Unknown local p : Promise#C<§R#G§> = <to>Promise\Promise#C.class\.<dIt>apply\Promise#C<§R#G§>\;
        onCompleteF(new P<Try<T>>() {
            @Override
            public void apply(Try<T> tr) {
                p.completeValue(tr.mapF<R>(f));
            }
        });
        return p;
    }
    public Future<Void> forF(P<T> f) {
        ERROR: Unknown local p : Promise#C<§^void§> = <to>Promise\Promise#C.class\.<dIt>apply\Promise#C<§^void§>\;
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
        ERROR: Unknown local p : Promise#C<§R#G§> = <to>Promise\Promise#C.class\.<dIt>apply\Promise#C<§R#G§>\;
        onCompleteF(new P<Try<T>>() {
            @Override
            public void apply(Try<T> tr) {
                if(tr.isFailure()) {
                    p.completeValue(tr.ERROR: Unknown cast<Try#C<R#G>>);
                } else {
                    ERROR: Unknown local fut : Future#C<R#G> = <l>f\§T#G§ -> Future#C<R#G>\.<d>apply( = <l>tr\Try#C<§T#G§>\.<dIa>get\§T#G§\)\Future#C<R#G>\;
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
        ERROR: Unknown local lock : ConditionLock#P = <to>ConditionLock\ConditionLock#P.class\.<tcI>apply(condition = 0)\ConditionLock#P\;
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
        ERROR: Unknown local lock : ConditionLock#P = <to>ConditionLock\ConditionLock#P.class\.<tcI>apply(condition = 0)\ConditionLock#P\;
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
        ERROR: Unknown local p : Promise#C<§^(T#G, R#G)§> = <to>Promise\Promise#C.class\.<dIt>apply\Promise#C<§^(T#G, R#G)§>\;
        ERROR: Unknown local var a : T#G = nil;
        ERROR: Unknown local var b : R#G = nil;
        ERROR: Unknown local n : AtomicInt#C = <to>AtomicInt\AtomicInt#C.class\.<tcI>apply\AtomicInt#C\;
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