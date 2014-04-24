package core.chain;

public class FutureVoidEnd<T> {
    private final Promise<Void> _promise = Promise.<Void>apply();
    private boolean _stopped = false;
    private AtomicInt _counter = new AtomicInt();
    private boolean _ended = false;
    private AtomicBool _yielded = new AtomicBool();
    public Future<Void> future() {
        return this._promise;
    }
    public Yield<Future<T>> yield() {
        return new Yield<Future<T>>(new F<Integer, Integer>() {
            @Override
            public Integer apply(Integer size) {
                return 0;
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
                                    FutureVoidEnd.this._stopped = true;
                                    FutureVoidEnd.this._promise.failureReason(tr);
                                } else {
                                    if(ERROR: Unknown !(<FutureVoidEnd#C<T#G>>self.<emp>_stopped\bool\)) {
                                        int r = FutureVoidEnd.this._counter.decrementAndGet();
                                        Memory.memoryBarrier();
                                        if(FutureVoidEnd.this._ended && r.equals(0)) {
                                            Memory.memoryBarrier();
                                            if(ERROR: Unknown !(<FutureVoidEnd#C<T#G>>self.<emp>_yielded\AtomicBool#C\.<dIb>getAndSet(newValue = True)\bool\)) {
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
                    return 1;
                } else {
                    return 0;
                }
            }
        }, new F<Integer, Integer>() {
            @Override
            public Integer apply(Integer res) {
                int ret = res;
                FutureVoidEnd.this._ended = true;
                Memory.memoryBarrier();
                if(FutureVoidEnd.this._counter.intValue().equals(0)) {
                    Memory.memoryBarrier();
                    if(ERROR: Unknown !(<FutureVoidEnd#C<T#G>>self.<emp>_yielded\AtomicBool#C\.<dIb>getAndSet(newValue = True)\bool\)) {
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