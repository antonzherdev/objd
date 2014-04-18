package core.chain;

public class Future<T> {
    public static Future<T> applyF(F<Void, T> f) {
        ERROR: Unknown local p : Promise#C<§T#G§> = <to>Promise\Promise#C.class\.<dIt>apply\Promise#C<§T#G§>\;
        DispatchQueue().default.asyncF(ERROR: Unknown  -> void = return <l>p\Promise#C<§T#G§>\.<dIa>success(value = <l>f\void -> T#G\())\bool\);
        return p;
    }
    public static Future<Tuple2<A, B>> joinAB(Future<A> a,Future<B> b) {
        return Future().mapABF<A, B, Tuple2<A, B>>(a, b, ERROR: Unknown _a : §A#G§, _b : §B#G§ -> ^(§A#G§, §B#G§) = return (<l>_a\§A#G§\, <l>_b\§B#G§\));
    }
    public static Future<Tuple3<A, B, C>> joinABC(Future<A> a,Future<B> b,Future<C> c) {
        return Future().mapABCF<A, B, C, Tuple3<A, B, C>>(a, b, c, ERROR: Unknown _a : §A#G§, _b : §B#G§, _c : §C#G§ -> ^(§A#G§, §B#G§, §C#G§) = return (<l>_a\§A#G§\, <l>_b\§B#G§\, <l>_c\§C#G§\));
    }
    public static Future<Tuple4<A, B, C, D>> joinABCD(Future<A> a,Future<B> b,Future<C> c,Future<D> d) {
        return Future().mapABCDF<A, B, C, D, Tuple4<A, B, C, D>>(a, b, c, d, ERROR: Unknown _a : §A#G§, _b : §B#G§, _c : §C#G§, _d : §D#G§ -> ^(§A#G§, §B#G§, §C#G§, §D#G§) = return (<l>_a\§A#G§\, <l>_b\§B#G§\, <l>_c\§C#G§\, <l>_d\§D#G§\));
    }
    public static Future<Tuple5<A, B, C, D, E>> joinABCDE(Future<A> a,Future<B> b,Future<C> c,Future<D> d,Future<E> e) {
        return Future().mapABCDEF<A, B, C, D, E, Tuple5<A, B, C, D, E>>(a, b, c, d, e, ERROR: Unknown _a : §A#G§, _b : §B#G§, _c : §C#G§, _d : §D#G§, _e : §E#G§ -> ^(§A#G§, §B#G§, §C#G§, §D#G§, §E#G§) = return (<l>_a\§A#G§\, <l>_b\§B#G§\, <l>_c\§C#G§\, <l>_d\§D#G§\, <l>_e\§E#G§\));
    }
    public static Future<R> mapABF(Future<A> a,Future<B> b,F2<A, B, R> f) {
        ERROR: Unknown local p : Promise#C<§R#G§> = <to>Promise\Promise#C.class\.<dIt>apply\Promise#C<§R#G§>\;
        ERROR: Unknown local var _a : A#G = nil;
        ERROR: Unknown local var _b : B#G = nil;
        ERROR: Unknown local n : AtomicInt#C = <to>AtomicInt\AtomicInt#C.class\.<tcI>apply\AtomicInt#C\;
        a.onCompleteF(ERROR: Unknown t : Try#C<A#G> -> void = if(<l>t\Try#C<A#G>\.<dIa>isSuccess\bool\) {
    (<lm>_a\A#G\ = <l>t\Try#C<A#G>\.<dIa>get\A#G\)
    <to>Memory\Memory#C.class\.<dIt>memoryBarrier\void\
    if((<l>n\AtomicInt#C\.<dI>incrementAndGet\int4\ == 2)) <l>p\Promise#C<§R#G§>\.<dIa>success(value = <l>f\(A#G, B#G) -> R#G\.<d>apply( = <lm>_a\A#G\,  = <lm>_b\B#G\)\R#G\)\bool\
}
else {
    <l>p\Promise#C<§R#G§>\.<dIa>complete(value = <l>t\Try#C<A#G>\.cast<Try#C<R#G>>)\bool\
});
        b.onCompleteF(ERROR: Unknown t : Try#C<B#G> -> void = if(<l>t\Try#C<B#G>\.<dIa>isSuccess\bool\) {
    (<lm>_b\B#G\ = <l>t\Try#C<B#G>\.<dIa>get\B#G\)
    <to>Memory\Memory#C.class\.<dIt>memoryBarrier\void\
    if((<l>n\AtomicInt#C\.<dI>incrementAndGet\int4\ == 2)) <l>p\Promise#C<§R#G§>\.<dIa>success(value = <l>f\(A#G, B#G) -> R#G\.<d>apply( = <lm>_a\A#G\,  = <lm>_b\B#G\)\R#G\)\bool\
}
else {
    <l>p\Promise#C<§R#G§>\.<dIa>complete(value = <l>t\Try#C<B#G>\.cast<Try#C<R#G>>)\bool\
});
        return p;
    }
    public static Future<R> mapABCF(Future<A> a,Future<B> b,Future<C> c,F3<A, B, C, R> f) {
        ERROR: Unknown local p : Promise#C<§R#G§> = <to>Promise\Promise#C.class\.<dIt>apply\Promise#C<§R#G§>\;
        ERROR: Unknown local var _a : A#G = nil;
        ERROR: Unknown local var _b : B#G = nil;
        ERROR: Unknown local var _c : C#G = nil;
        ERROR: Unknown local n : AtomicInt#C = <to>AtomicInt\AtomicInt#C.class\.<tcI>apply\AtomicInt#C\;
        a.onCompleteF(ERROR: Unknown t : Try#C<A#G> -> void = if(<l>t\Try#C<A#G>\.<dIa>isSuccess\bool\) {
    (<lm>_a\A#G\ = <l>t\Try#C<A#G>\.<dIa>get\A#G\)
    <to>Memory\Memory#C.class\.<dIt>memoryBarrier\void\
    if((<l>n\AtomicInt#C\.<dI>incrementAndGet\int4\ == 3)) <l>p\Promise#C<§R#G§>\.<dIa>success(value = <l>f\(A#G, B#G, C#G) -> R#G\.<d>apply( = <lm>_a\A#G\,  = <lm>_b\B#G\,  = <lm>_c\C#G\)\R#G\)\bool\
}
else {
    <l>p\Promise#C<§R#G§>\.<dIa>complete(value = <l>t\Try#C<A#G>\.cast<Try#C<R#G>>)\bool\
});
        b.onCompleteF(ERROR: Unknown t : Try#C<B#G> -> void = if(<l>t\Try#C<B#G>\.<dIa>isSuccess\bool\) {
    (<lm>_b\B#G\ = <l>t\Try#C<B#G>\.<dIa>get\B#G\)
    <to>Memory\Memory#C.class\.<dIt>memoryBarrier\void\
    if((<l>n\AtomicInt#C\.<dI>incrementAndGet\int4\ == 3)) <l>p\Promise#C<§R#G§>\.<dIa>success(value = <l>f\(A#G, B#G, C#G) -> R#G\.<d>apply( = <lm>_a\A#G\,  = <lm>_b\B#G\,  = <lm>_c\C#G\)\R#G\)\bool\
}
else {
    <l>p\Promise#C<§R#G§>\.<dIa>complete(value = <l>t\Try#C<B#G>\.cast<Try#C<R#G>>)\bool\
});
        c.onCompleteF(ERROR: Unknown t : Try#C<C#G> -> void = if(<l>t\Try#C<C#G>\.<dIa>isSuccess\bool\) {
    (<lm>_c\C#G\ = <l>t\Try#C<C#G>\.<dIa>get\C#G\)
    <to>Memory\Memory#C.class\.<dIt>memoryBarrier\void\
    if((<l>n\AtomicInt#C\.<dI>incrementAndGet\int4\ == 3)) <l>p\Promise#C<§R#G§>\.<dIa>success(value = <l>f\(A#G, B#G, C#G) -> R#G\.<d>apply( = <lm>_a\A#G\,  = <lm>_b\B#G\,  = <lm>_c\C#G\)\R#G\)\bool\
}
else {
    <l>p\Promise#C<§R#G§>\.<dIa>complete(value = <l>t\Try#C<C#G>\.cast<Try#C<R#G>>)\bool\
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
        a.onCompleteF(ERROR: Unknown t : Try#C<A#G> -> void = if(<l>t\Try#C<A#G>\.<dIa>isSuccess\bool\) {
    (<lm>_a\A#G\ = <l>t\Try#C<A#G>\.<dIa>get\A#G\)
    <to>Memory\Memory#C.class\.<dIt>memoryBarrier\void\
    if((<l>n\AtomicInt#C\.<dI>incrementAndGet\int4\ == 4)) <l>p\Promise#C<§R#G§>\.<dIa>success(value = <l>f\(A#G, B#G, C#G, D#G) -> R#G\.<d>apply( = <lm>_a\A#G\,  = <lm>_b\B#G\,  = <lm>_c\C#G\,  = <lm>_d\D#G\)\R#G\)\bool\
}
else {
    <l>p\Promise#C<§R#G§>\.<dIa>complete(value = <l>t\Try#C<A#G>\.cast<Try#C<R#G>>)\bool\
});
        b.onCompleteF(ERROR: Unknown t : Try#C<B#G> -> void = if(<l>t\Try#C<B#G>\.<dIa>isSuccess\bool\) {
    (<lm>_b\B#G\ = <l>t\Try#C<B#G>\.<dIa>get\B#G\)
    <to>Memory\Memory#C.class\.<dIt>memoryBarrier\void\
    if((<l>n\AtomicInt#C\.<dI>incrementAndGet\int4\ == 4)) <l>p\Promise#C<§R#G§>\.<dIa>success(value = <l>f\(A#G, B#G, C#G, D#G) -> R#G\.<d>apply( = <lm>_a\A#G\,  = <lm>_b\B#G\,  = <lm>_c\C#G\,  = <lm>_d\D#G\)\R#G\)\bool\
}
else {
    <l>p\Promise#C<§R#G§>\.<dIa>complete(value = <l>t\Try#C<B#G>\.cast<Try#C<R#G>>)\bool\
});
        c.onCompleteF(ERROR: Unknown t : Try#C<C#G> -> void = if(<l>t\Try#C<C#G>\.<dIa>isSuccess\bool\) {
    (<lm>_c\C#G\ = <l>t\Try#C<C#G>\.<dIa>get\C#G\)
    <to>Memory\Memory#C.class\.<dIt>memoryBarrier\void\
    if((<l>n\AtomicInt#C\.<dI>incrementAndGet\int4\ == 4)) <l>p\Promise#C<§R#G§>\.<dIa>success(value = <l>f\(A#G, B#G, C#G, D#G) -> R#G\.<d>apply( = <lm>_a\A#G\,  = <lm>_b\B#G\,  = <lm>_c\C#G\,  = <lm>_d\D#G\)\R#G\)\bool\
}
else {
    <l>p\Promise#C<§R#G§>\.<dIa>complete(value = <l>t\Try#C<C#G>\.cast<Try#C<R#G>>)\bool\
});
        d.onCompleteF(ERROR: Unknown t : Try#C<D#G> -> void = if(<l>t\Try#C<D#G>\.<dIa>isSuccess\bool\) {
    (<lm>_d\D#G\ = <l>t\Try#C<D#G>\.<dIa>get\D#G\)
    <to>Memory\Memory#C.class\.<dIt>memoryBarrier\void\
    if((<l>n\AtomicInt#C\.<dI>incrementAndGet\int4\ == 4)) <l>p\Promise#C<§R#G§>\.<dIa>success(value = <l>f\(A#G, B#G, C#G, D#G) -> R#G\.<d>apply( = <lm>_a\A#G\,  = <lm>_b\B#G\,  = <lm>_c\C#G\,  = <lm>_d\D#G\)\R#G\)\bool\
}
else {
    <l>p\Promise#C<§R#G§>\.<dIa>complete(value = <l>t\Try#C<D#G>\.cast<Try#C<R#G>>)\bool\
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
        a.onCompleteF(ERROR: Unknown t : Try#C<A#G> -> void = if(<l>t\Try#C<A#G>\.<dIa>isSuccess\bool\) {
    (<lm>_a\A#G\ = <l>t\Try#C<A#G>\.<dIa>get\A#G\)
    <to>Memory\Memory#C.class\.<dIt>memoryBarrier\void\
    if((<l>n\AtomicInt#C\.<dI>incrementAndGet\int4\ == 5)) <l>p\Promise#C<§R#G§>\.<dIa>success(value = <l>f\(A#G, B#G, C#G, D#G, E#G) -> R#G\.<d>apply( = <lm>_a\A#G\,  = <lm>_b\B#G\,  = <lm>_c\C#G\,  = <lm>_d\D#G\,  = <lm>_e\D#G\)\R#G\)\bool\
}
else {
    <l>p\Promise#C<§R#G§>\.<dIa>complete(value = <l>t\Try#C<A#G>\.cast<Try#C<R#G>>)\bool\
});
        b.onCompleteF(ERROR: Unknown t : Try#C<B#G> -> void = if(<l>t\Try#C<B#G>\.<dIa>isSuccess\bool\) {
    (<lm>_b\B#G\ = <l>t\Try#C<B#G>\.<dIa>get\B#G\)
    <to>Memory\Memory#C.class\.<dIt>memoryBarrier\void\
    if((<l>n\AtomicInt#C\.<dI>incrementAndGet\int4\ == 5)) <l>p\Promise#C<§R#G§>\.<dIa>success(value = <l>f\(A#G, B#G, C#G, D#G, E#G) -> R#G\.<d>apply( = <lm>_a\A#G\,  = <lm>_b\B#G\,  = <lm>_c\C#G\,  = <lm>_d\D#G\,  = <lm>_e\D#G\)\R#G\)\bool\
}
else {
    <l>p\Promise#C<§R#G§>\.<dIa>complete(value = <l>t\Try#C<B#G>\.cast<Try#C<R#G>>)\bool\
});
        c.onCompleteF(ERROR: Unknown t : Try#C<C#G> -> void = if(<l>t\Try#C<C#G>\.<dIa>isSuccess\bool\) {
    (<lm>_c\C#G\ = <l>t\Try#C<C#G>\.<dIa>get\C#G\)
    <to>Memory\Memory#C.class\.<dIt>memoryBarrier\void\
    if((<l>n\AtomicInt#C\.<dI>incrementAndGet\int4\ == 5)) <l>p\Promise#C<§R#G§>\.<dIa>success(value = <l>f\(A#G, B#G, C#G, D#G, E#G) -> R#G\.<d>apply( = <lm>_a\A#G\,  = <lm>_b\B#G\,  = <lm>_c\C#G\,  = <lm>_d\D#G\,  = <lm>_e\D#G\)\R#G\)\bool\
}
else {
    <l>p\Promise#C<§R#G§>\.<dIa>complete(value = <l>t\Try#C<C#G>\.cast<Try#C<R#G>>)\bool\
});
        d.onCompleteF(ERROR: Unknown t : Try#C<D#G> -> void = if(<l>t\Try#C<D#G>\.<dIa>isSuccess\bool\) {
    (<lm>_d\D#G\ = <l>t\Try#C<D#G>\.<dIa>get\D#G\)
    <to>Memory\Memory#C.class\.<dIt>memoryBarrier\void\
    if((<l>n\AtomicInt#C\.<dI>incrementAndGet\int4\ == 5)) <l>p\Promise#C<§R#G§>\.<dIa>success(value = <l>f\(A#G, B#G, C#G, D#G, E#G) -> R#G\.<d>apply( = <lm>_a\A#G\,  = <lm>_b\B#G\,  = <lm>_c\C#G\,  = <lm>_d\D#G\,  = <lm>_e\D#G\)\R#G\)\bool\
}
else {
    <l>p\Promise#C<§R#G§>\.<dIa>complete(value = <l>t\Try#C<D#G>\.cast<Try#C<R#G>>)\bool\
});
        e.onCompleteF(ERROR: Unknown t : Try#C<E#G> -> void = if(<l>t\Try#C<E#G>\.<dIa>isSuccess\bool\) {
    (<lm>_e\D#G\ = <l>t\Try#C<E#G>\.<dIa>get\E#G\)
    <to>Memory\Memory#C.class\.<dIt>memoryBarrier\void\
    if((<l>n\AtomicInt#C\.<dI>incrementAndGet\int4\ == 5)) <l>p\Promise#C<§R#G§>\.<dIa>success(value = <l>f\(A#G, B#G, C#G, D#G, E#G) -> R#G\.<d>apply( = <lm>_a\A#G\,  = <lm>_b\B#G\,  = <lm>_c\C#G\,  = <lm>_d\D#G\,  = <lm>_e\D#G\)\R#G\)\bool\
}
else {
    <l>p\Promise#C<§R#G§>\.<dIa>complete(value = <l>t\Try#C<E#G>\.cast<Try#C<R#G>>)\bool\
});
        return p;
    }
    public static Future<T> successfulResult(T result) {
        return new KeptPromise<T>(new Success<T>(result));
    }
    public abstract Try<T> result();
    public boolean isCompleted() {
        return ERROR: Unknown (<Future#C<T#G>>self.<dIa>result\(^Try#C<§T#G§>)?\ != none<^Try#C<§T#G§>>);
    }
    public boolean isSucceeded() {
        ERROR: Unknown local __tmp : (^Try#C<§T#G§>)? = <Future#C<T#G>>self.<dIa>result\(^Try#C<§T#G§>)?\;
        ERROR: Unknown if((<l>__tmp\(^Try#C<§T#G§>)?\ != none<(^Try#C<§T#G§>)?>)) return <Future#C<T#G>>self.<dIa>result\(^Try#C<§T#G§>)?\.get.<dIa>isSuccess\bool\
else return False;
    }
    public boolean isFailed() {
        ERROR: Unknown local __tmp : (^Try#C<§T#G§>)? = <Future#C<T#G>>self.<dIa>result\(^Try#C<§T#G§>)?\;
        ERROR: Unknown if((<l>__tmp\(^Try#C<§T#G§>)?\ != none<(^Try#C<§T#G§>)?>)) return <Future#C<T#G>>self.<dIa>result\(^Try#C<§T#G§>)?\.get.<dI>isFailure\bool\
else return True;
    }
    public abstract void onCompleteF(F<Try<T>, Void> f);
    public void onSuccessF(F<T, Void> f) {
        onCompleteF(ERROR: Unknown t : Try#C<§T#G§> -> void = if(<l>t\Try#C<§T#G§>\.<dIa>isSuccess\bool\) <l>f\§T#G§ -> void\.<d>apply( = <l>t\Try#C<§T#G§>\.<dIa>get\§T#G§\)\void\);
    }
    public void onFailureF(F<Object, Void> f) {
        onCompleteF(ERROR: Unknown t : Try#C<§T#G§> -> void = if(<l>t\Try#C<§T#G§>\.<dI>isFailure\bool\) <l>f\any -> void\.<d>apply( = <l>t\Try#C<§T#G§>\.<dIa>reason\any\)\void\);
    }
    public Future<R> mapF(F<T, R> f) {
        ERROR: Unknown local p : Promise#C<§R#G§> = <to>Promise\Promise#C.class\.<dIt>apply\Promise#C<§R#G§>\;
        onCompleteF(ERROR: Unknown tr : Try#C<§T#G§> -> void = <l>p\Promise#C<§R#G§>\.<dIa>complete(value = <l>tr\Try#C<§T#G§>\.<dIa>map(f = <l>f\§T#G§ -> R#G\)\Try#C<§R#G§>\)\bool\);
        return p;
    }
    public Future<Void> forF(F<T, Void> f) {
        ERROR: Unknown local p : Promise#C<§^void§> = <to>Promise\Promise#C.class\.<dIt>apply\Promise#C<§^void§>\;
        onCompleteF(ERROR: Unknown tr : Try#C<§T#G§> -> void = if(<l>tr\Try#C<§T#G§>\.<dIa>isSuccess\bool\) {
    <l>f\§T#G§ -> void\.<d>apply( = <l>tr\Try#C<§T#G§>\.<dIa>get\§T#G§\)\void\
    <l>p\Promise#C<§^void§>\.<dIa>success(value = nil)\bool\
}
else <l>p\Promise#C<§^void§>\.<dIa>complete(value = <l>tr\Try#C<§T#G§>\.cast<Try#C<^void>>)\bool\);
        return p;
    }
    public Future<R> flatMapF(F<T, Future<R>> f) {
        ERROR: Unknown local p : Promise#C<§R#G§> = <to>Promise\Promise#C.class\.<dIt>apply\Promise#C<§R#G§>\;
        onCompleteF(ERROR: Unknown tr : Try#C<§T#G§> -> void = if(<l>tr\Try#C<§T#G§>\.<dI>isFailure\bool\) {
    <l>p\Promise#C<§R#G§>\.<dIa>complete(value = <l>tr\Try#C<§T#G§>\.cast<Try#C<R#G>>)\bool\
}
else {
    local fut : Future#C<R#G> = <l>f\§T#G§ -> Future#C<R#G>\.<d>apply( = <l>tr\Try#C<§T#G§>\.<dIa>get\§T#G§\)\Future#C<R#G>\
    <l>fut\Future#C<R#G>\.<dIa>onComplete(f = ftr : Try#C<R#G> -> void = <l>p\Promise#C<§R#G§>\.<dIa>complete(value = <l>ftr\Try#C<R#G>\)\bool\)\void\
});
        return p;
    }
    public Try<T> waitResultPeriod(float period) {
        ERROR: Unknown local lock : ConditionLock#P = <to>ConditionLock\ConditionLock#P.class\.<tcI>apply(condition = 0)\ConditionLock#P\;
        onCompleteF(ERROR: Unknown _ : Try#C<§T#G§> -> void = {
    <l>lock\ConditionLock#P\.<rdI>lock\void\
    <l>lock\ConditionLock#P\.<rdI>unlockWith(condition = 1)\void\
});
        ERROR: Unknown if(<l>lock\ConditionLock#P\.<rdI>lockWhen(condition = 1, period = <l>period\float\)\bool\) {
    <l>lock\ConditionLock#P\.<rdI>unlock\void\
};
        return result();
    }
    public Try<T> waitResult() {
        ERROR: Unknown local lock : ConditionLock#P = <to>ConditionLock\ConditionLock#P.class\.<tcI>apply(condition = 0)\ConditionLock#P\;
        onCompleteF(ERROR: Unknown _ : Try#C<§T#G§> -> void = {
    <l>lock\ConditionLock#P\.<rdI>lock\void\
    <l>lock\ConditionLock#P\.<rdI>unlockWith(condition = 1)\void\
});
        lock.lockWhenCondition(ERROR: Unknown 1);
        lock.unlock();
        return ERROR: Unknown <Future#C<T#G>>self.<dIa>result\(^Try#C<§T#G§>)?\.get;
    }
    public void waitAndOnSuccessAwaitF(float await,F<T, Void> f) {
        {
            ERROR: Unknown local __tr : ^(^Try#C<§T#G§>)¿ = <Future#C<T#G>>self.<dI>waitResult(period = <l>await\float\)\(^Try#C<§T#G§>)?\;
            ERROR: Unknown if((<l>__tr\^(^Try#C<§T#G§>)¿\ != none<^Try#C<§T#G§>>)) if(<l>__tr\^(^Try#C<§T#G§>)¿\.<dIa>isSuccess\bool\) {
    <l>f\§T#G§ -> void\.<d>apply( = <l>__tr\^(^Try#C<§T#G§>)¿\.<dIa>get\§T#G§\)\void\
};
        }
    }
    public void waitAndOnSuccessFlatAwaitF(float await,F<I, Void> f) {
        {
            {
                ERROR: Unknown local __inline__0___tr : ^(^Try#C<T#G>)¿ = <Future#C<T#G>>self.<dI>waitResult(period = <l>await\float\)\(^Try#C<§T#G§>)?\;
                ERROR: Unknown if((<l>__inline__0___tr\^(^Try#C<T#G>)¿\ != none<^Try#C<§T#G§>>)) if(<l>__inline__0___tr\^(^Try#C<T#G>)¿\.<dIa>isSuccess\bool\) {
    {
    local __tr2 : §T#G§ = <l>__inline__0___tr\^(^Try#C<T#G>)¿\.<dIa>get\§T#G§\
    <l>__tr2\§T#G§\.cast<Traversable#T<I#G>>.<dI>for(each = <l>f\I#G -> void\)\void\
}
};
            }
        }
    }
    public T getResultAwait(float await) {
        return ERROR: Unknown <Future#C<T#G>>self.<dI>waitResult(period = <l>await\float\)\(^Try#C<§T#G§>)?\?.<dIa>get\§T#G§\.get;
    }
    public Future<Tuple2<T, R>> joinAnother(Future<R> another) {
        ERROR: Unknown local p : Promise#C<§^(T#G, R#G)§> = <to>Promise\Promise#C.class\.<dIt>apply\Promise#C<§^(T#G, R#G)§>\;
        ERROR: Unknown local var a : T#G = nil;
        ERROR: Unknown local var b : R#G = nil;
        ERROR: Unknown local n : AtomicInt#C = <to>AtomicInt\AtomicInt#C.class\.<tcI>apply\AtomicInt#C\;
        onCompleteF(ERROR: Unknown t : Try#C<§T#G§> -> void = if(<l>t\Try#C<§T#G§>\.<dIa>isSuccess\bool\) {
    (<lm>a\§T#G§\ = <l>t\Try#C<§T#G§>\.<dIa>get\§T#G§\)
    <to>Memory\Memory#C.class\.<dIt>memoryBarrier\void\
    if((<l>n\AtomicInt#C\.<dI>incrementAndGet\int4\ == 2)) <l>p\Promise#C<§^(T#G, R#G)§>\.<dIa>success(value = <to>Tuple\Tuple#C.class\.<tcI>apply(a = <lm>a\§T#G§\, b = <lm>b\R#G\)\Tuple#C<§T#G§, §R#G§>\)\bool\
}
else {
    <l>p\Promise#C<§^(T#G, R#G)§>\.<dIa>complete(value = <l>t\Try#C<§T#G§>\.cast<Try#C<^(T#G, R#G)>>)\bool\
});
        another.onCompleteF(ERROR: Unknown t : Try#C<R#G> -> void = if(<l>t\Try#C<R#G>\.<dIa>isSuccess\bool\) {
    (<lm>b\R#G\ = <l>t\Try#C<R#G>\.<dIa>get\R#G\)
    <to>Memory\Memory#C.class\.<dIt>memoryBarrier\void\
    if((<l>n\AtomicInt#C\.<dI>incrementAndGet\int4\ == 2)) <l>p\Promise#C<§^(T#G, R#G)§>\.<dIa>success(value = <to>Tuple\Tuple#C.class\.<tcI>apply(a = <lm>a\§T#G§\, b = <lm>b\R#G\)\Tuple#C<§T#G§, §R#G§>\)\bool\
}
else {
    <l>p\Promise#C<§^(T#G, R#G)§>\.<dIa>complete(value = <l>t\Try#C<R#G>\.cast<Try#C<^(T#G, R#G)>>)\bool\
});
        return p;
    }
    public Future() {
    }
    static final ClassType<Future<T>> type;
}