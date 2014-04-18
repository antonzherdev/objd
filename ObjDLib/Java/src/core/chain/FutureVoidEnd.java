package core.chain;

public class FutureVoidEnd<T> {
    private final Promise<Void> _promise = Promise().apply<Void>();
    private boolean _stopped = ERROR: Unknown False;
    private AtomicInt _counter = new AtomicInt();
    private boolean _ended = ERROR: Unknown False;
    private AtomicBool _yielded = new AtomicBool();
    public Future<Void> future() {
        return _promise;
    }
    public Yield<Future<T>> yield() {
        return Yield().applyBeginYieldEnd<Future<T>>(new F<Integer, Integer>() {
            @Override
            public Integer f(Integer size) {
                return ERROR: Unknown 0;
            }
        }, new F<Future<T>, Integer>() {
            @Override
            public Integer f(Future<T> fut) {
                ERROR: Unknown if(!(<FutureVoidEnd#C<T#G>>self.<emp>_stopped\bool\)) {
    <FutureVoidEnd#C<T#G>>self.<emp>_counter\AtomicInt#C\.<dI>incrementAndGet\int4\
    <l>fut\§^Future#C<T#G>§\.<dIa>onComplete(f = tr : Try#C<§T#G§> -> void = if(!(<FutureVoidEnd#C<T#G>>self.<emp>_stopped\bool\)) {
    if(<l>tr\Try#C<§T#G§>\.<dI>isFailure\bool\) {
    (<FutureVoidEnd#C<T#G>>self.<emp>_stopped\bool\ = True)
    <FutureVoidEnd#C<T#G>>self.<ep>_promise\Promise#C<§^void§>\.<dIa>failure(reason = <l>tr\Try#C<§T#G§>\)\bool\
}
else if(!(<FutureVoidEnd#C<T#G>>self.<emp>_stopped\bool\)) {
    local r : int4 = <FutureVoidEnd#C<T#G>>self.<emp>_counter\AtomicInt#C\.<dI>decrementAndGet\int4\
    <to>Memory\Memory#C.class\.<dIt>memoryBarrier\void\
    if((<FutureVoidEnd#C<T#G>>self.<emp>_ended\bool\ && (<l>r\int4\ == 0))) {
    <to>Memory\Memory#C.class\.<dIt>memoryBarrier\void\
    if(!(<FutureVoidEnd#C<T#G>>self.<emp>_yielded\AtomicBool#C\.<dI>getAndSet(newValue = True)\bool\)) {
    <FutureVoidEnd#C<T#G>>self.<ep>_promise\Promise#C<§^void§>\.<dIa>success(value = nil)\bool\
}
}
}
})\void\
};
                ERROR: Unknown if(<FutureVoidEnd#C<T#G>>self.<emp>_stopped\bool\) return 1
else return 0;
            }
        }, new F<Integer, Integer>() {
            @Override
            public Integer f(Integer res) {
                ERROR: Unknown local var ret : int = <l>res\int\;
                ERROR: Unknown (<FutureVoidEnd#C<T#G>>self.<emp>_ended\bool\ = True);
                Memory().memoryBarrier();
                ERROR: Unknown if((<FutureVoidEnd#C<T#G>>self.<emp>_counter\AtomicInt#C\.<dI>intValue\int4\ == 0)) {
    <to>Memory\Memory#C.class\.<dIt>memoryBarrier\void\
    if(!(<FutureVoidEnd#C<T#G>>self.<emp>_yielded\AtomicBool#C\.<dI>getAndSet(newValue = True)\bool\)) {
    <FutureVoidEnd#C<T#G>>self.<ep>_promise\Promise#C<§^void§>\.<dIa>success(value = nil)\bool\
}
};
                return ret;
            }
        });
    }
    public FutureVoidEnd() {
    }
}