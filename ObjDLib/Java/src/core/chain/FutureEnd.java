package core.chain;

public class FutureEnd<T> {
    private final Promise<Seq<T>> _promise = Promise().apply<Seq<T>>();
    private boolean _stopped = false;
    private AtomicInt _counter = new AtomicInt();
    private boolean _ended = false;
    private AtomicBool _yielded = new AtomicBool();
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
                if(ERROR: Unknown !(<FutureEnd#C<T#G>>self.<emp>_stopped\bool\)) {
                    FutureEnd.this._counter.incrementAndGet();
                    if(FutureEnd.this._array == null) {
                        throw new RuntimeException("Not null");
                    } else {
                        FutureEnd.this._array;
                    }
                    .appendItem(null);
                    int i = _i;
                    ERROR: Unknown <lm>_i\int\++;
                    fut.onCompleteF(new P<Try<T>>() {
                        @Override
                        public void apply(Try<T> tr) {
                            if(ERROR: Unknown !(<FutureEnd#C<T#G>>self.<emp>_stopped\bool\)) {
                                if(tr.isFailure()) {
                                    FutureEnd.this._stopped = true;
                                    FutureEnd.this._promise.failureReason(tr);
                                } else {
                                    if(ERROR: Unknown !(<FutureEnd#C<T#G>>self.<emp>_stopped\bool\)) {
                                        if(FutureEnd.this._array == null) {
                                            throw new RuntimeException("Not null");
                                        } else {
                                            FutureEnd.this._array;
                                        }
                                        .setIndexItem(ERROR: Unknown <l>i\int\.cast<uint>, tr.get());
                                        Memory().memoryBarrier();
                                        int r = FutureEnd.this._counter.decrementAndGet();
                                        Memory().memoryBarrier();
                                        if(FutureEnd.this._ended && r.equals(0)) {
                                            Memory().memoryBarrier();
                                            if(ERROR: Unknown !(<FutureEnd#C<T#G>>self.<emp>_yielded\AtomicBool#C\.<dIb>getAndSet(newValue = True)\bool\)) {
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
                Memory().memoryBarrier();
                if(FutureEnd.this._counter.intValue().equals(0)) {
                    Memory().memoryBarrier();
                    if(ERROR: Unknown !(<FutureEnd#C<T#G>>self.<emp>_yielded\AtomicBool#C\.<dIb>getAndSet(newValue = True)\bool\)) {
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
    }
}