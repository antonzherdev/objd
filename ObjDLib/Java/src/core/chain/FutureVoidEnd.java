package core.chain;

public class FutureVoidEnd<T> {
    private final Promise<Void> _promise;
    private boolean _stopped;
    private AtomicInt _counter;
    private boolean _ended;
    private AtomicBool _yielded;
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
                if(!(FutureVoidEnd.this._stopped)) {
                    FutureVoidEnd.this._counter.incrementAndGet();
                    fut.onCompleteF(new P<Try<T>>() {
                        @Override
                        public void apply(Try<T> tr) {
                            if(!(FutureVoidEnd.this._stopped)) {
                                if(tr.isFailure()) {
                                    FutureVoidEnd.this._stopped = true;
                                    FutureVoidEnd.this._promise.failureReason(tr);
                                } else {
                                    if(!(FutureVoidEnd.this._stopped)) {
                                        int r = FutureVoidEnd.this._counter.decrementAndGet();
                                        Memory.memoryBarrier();
                                        if(FutureVoidEnd.this._ended && r.equals(0)) {
                                            Memory.memoryBarrier();
                                            if(!(FutureVoidEnd.this._yielded.getAndSetNewValue(true))) {
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
                    if(!(FutureVoidEnd.this._yielded.getAndSetNewValue(true))) {
                        FutureVoidEnd.this._promise.successValue(null);
                    }
                }
                return ret;
            }
        });
    }
    public FutureVoidEnd() {
        this._promise = Promise.<Void>apply();
        this._stopped = false;
        this._counter = new AtomicInt();
        this._ended = false;
        this._yielded = new AtomicBool();
    }
}