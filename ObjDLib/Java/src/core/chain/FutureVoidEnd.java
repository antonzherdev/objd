package core.chain;

public class FutureVoidEnd<T> {
    private final Promise<Void> _promise = Promise().apply<Void>();
    private boolean _stopped = ERROR: Unknown False;
    private AtomicInt _counter = new AtomicInt();
    private boolean _ended = ERROR: Unknown False;
    private AtomicBool _yielded = new AtomicBool();
    public Future<Void> future() {
        return this._promise;
    }
    public Yield<Future<T>> yield() {
        return Yield().applyBeginYieldEnd<Future<T>>(new F<Integer, Integer>() {
            @Override
            public Integer apply(Integer size) {
                return ERROR: Unknown 0;
            }
        }, new F<Future<T>, Integer>() {
            @Override
            public Integer apply(Future<T> fut) {
                if(ERROR: Unknown !(<FutureVoidEnd#C<T#G>>self.<emp>_stopped\bool\)) {
                    FutureVoidEnd.this._counter.incrementAndGet();
                    fut.onCompleteF(new P<Try<T>>() {
                        @Override
                        public void apply(Try<T> tr) {
                            if(ERROR: Unknown !(<FutureVoidEnd#C<T#G>>self.<emp>_stopped\bool\)) {
                                if(tr.isFailure()) {
                                    FutureVoidEnd.this._stopped = ERROR: Unknown True;
                                    FutureVoidEnd.this._promise.failureReason(tr);
                                } else {
                                    if(ERROR: Unknown !(<FutureVoidEnd#C<T#G>>self.<emp>_stopped\bool\)) {
                                        ERROR: Unknown local r : int4 = <FutureVoidEnd#C<T#G>>self.<emp>_counter\AtomicInt#C\.<dI>decrementAndGet\int4\;
                                        Memory().memoryBarrier();
                                        if(FutureVoidEnd.this._ended && r.equals(ERROR: Unknown 0)) {
                                            Memory().memoryBarrier();
                                            if(ERROR: Unknown !(<FutureVoidEnd#C<T#G>>self.<emp>_yielded\AtomicBool#C\.<dI>getAndSet(newValue = True)\bool\)) {
                                                FutureVoidEnd.this._promise.successValue(null);
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    });
                }
                if(FutureVoidEnd.this._stopped) {
                    return ERROR: Unknown 1;
                } else {
                    return ERROR: Unknown 0;
                }
            }
        }, new F<Integer, Integer>() {
            @Override
            public Integer apply(Integer res) {
                ERROR: Unknown local var ret : int = <l>res\int\;
                FutureVoidEnd.this._ended = ERROR: Unknown True;
                Memory().memoryBarrier();
                if(FutureVoidEnd.this._counter.intValue().equals(ERROR: Unknown 0)) {
                    Memory().memoryBarrier();
                    if(ERROR: Unknown !(<FutureVoidEnd#C<T#G>>self.<emp>_yielded\AtomicBool#C\.<dI>getAndSet(newValue = True)\bool\)) {
                        FutureVoidEnd.this._promise.successValue(null);
                    }
                }
                return ret;
            }
        });
    }
    public FutureVoidEnd() {
    }
}