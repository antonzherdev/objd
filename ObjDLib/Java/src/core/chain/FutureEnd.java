package core.chain;

public class FutureEnd<T> {
    private final Promise<Seq<T>> _promise;
    private boolean _stopped;
    private AtomicInt _counter;
    private boolean _ended;
    private AtomicBool _yielded;
    private MArray<T> _array;
    public Future<Seq<T>> future() {
        return this._promise;
    }
    public Yield<Future<T>> yield() {
        int _i = 0;
        return new Yield<Future<T>>(new F<Integer, Integer>() {
            @Override
            public Integer apply(Integer size) {
                FutureEnd.this._array = new MArray<T>(size);
                return 0;
            }
        }, new F<Future<T>, Integer>() {
            @Override
            public Integer apply(Future<T> fut) {
                if(!(FutureEnd.this._stopped)) {
                    FutureEnd.this._counter.incrementAndGet();
                    if(FutureEnd.this._array == null) {
                        throw new RuntimeException("Not null");
                    } else {
                        FutureEnd.this._array;
                    }
                    .appendItem(null);
                    int i = _i;
                    _i++;
                    fut.onCompleteF(new P<Try<T>>() {
                        @Override
                        public void apply(Try<T> tr) {
                            if(!(FutureEnd.this._stopped)) {
                                if(tr.isFailure()) {
                                    FutureEnd.this._stopped = true;
                                    FutureEnd.this._promise.failureReason(tr);
                                } else {
                                    if(!(FutureEnd.this._stopped)) {
                                        if(FutureEnd.this._array == null) {
                                            throw new RuntimeException("Not null");
                                        } else {
                                            FutureEnd.this._array;
                                        }
                                        .setIndexItem(((int)i), tr.get());
                                        Memory.memoryBarrier();
                                        int r = FutureEnd.this._counter.decrementAndGet();
                                        Memory.memoryBarrier();
                                        if(FutureEnd.this._ended && r.equals(0)) {
                                            Memory.memoryBarrier();
                                            if(!(FutureEnd.this._yielded.getAndSetNewValue(true))) {
                                                if(FutureEnd.this._array == null) {
                                                    throw new RuntimeException("Not null");
                                                } else {
                                                    FutureEnd.this._array;
                                                }
                                                FutureEnd.this._promise.successValue();
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    });
                }
                if(FutureEnd.this._stopped) {
                    return 1;
                } else {
                    return 0;
                }
            }
        }, new F<Integer, Integer>() {
            @Override
            public Integer apply(Integer res) {
                FutureEnd.this._ended = true;
                Memory.memoryBarrier();
                if(FutureEnd.this._counter.intValue().equals(0)) {
                    Memory.memoryBarrier();
                    if(!(FutureEnd.this._yielded.getAndSetNewValue(true))) {
                        if(FutureEnd.this._array == null) {
                            throw new RuntimeException("Not null");
                        } else {
                            FutureEnd.this._array;
                        }
                        FutureEnd.this._promise.successValue();
                    }
                }
                return res;
            }
        });
    }
    public FutureEnd() {
        this._promise = Promise.<Seq<T>>apply();
        this._stopped = false;
        this._counter = new AtomicInt();
        this._ended = false;
        this._yielded = new AtomicBool();
    }
}