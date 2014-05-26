package objd.chain;

import objd.lang.*;
import objd.concurrent.Promise;
import objd.collection.ImArray;
import objd.concurrent.AtomicInt;
import objd.concurrent.AtomicBool;
import objd.collection.MArray;
import objd.concurrent.Future;
import objd.collection.Go;

public class FutureEnd<T> {
    private final Promise<ImArray<T>> _promise;
    private boolean _stopped;
    private AtomicInt _counter;
    private volatile boolean _ended;
    private AtomicBool _yielded;
    private MArray<T> _array;
    public Future<ImArray<T>> future() {
        return this._promise;
    }
    public Yield<Future<T>> yield() {
        final Mut<Integer> _i = new Mut<Integer>(0);
        final MutVolatile<Integer> _set = new MutVolatile<Integer>(-1);
        return Yield.<Future<T>>makeBeginYieldEnd(new F<Integer, Go>() {
            @Override
            public Go apply(final Integer size) {
                FutureEnd.this._array = new MArray<T>(size);
                return Go.Continue;
            }
        }, new F<Future<T>, Go>() {
            @Override
            public Go apply(final Future<T> fut) {
                if(!(FutureEnd.this._stopped)) {
                    FutureEnd.this._counter.incrementAndGet();
                    if(FutureEnd.this._array == null) {
                        throw new NullPointerException();
                    }
                    FutureEnd.this._array.appendItem(null);
                    final int i = _i.value;
                    _i.value++;
                    fut.onCompleteF(new P<Try<T>>() {
                        @Override
                        public void apply(final Try<T> tr) {
                            if(!(FutureEnd.this._stopped)) {
                                if(tr.isFailure()) {
                                    FutureEnd.this._stopped = true;
                                    FutureEnd.this._promise.failureReason(tr);
                                } else {
                                    if(!(FutureEnd.this._stopped)) {
                                        if(FutureEnd.this._array == null) {
                                            throw new NullPointerException();
                                        }
                                        FutureEnd.this._array.setIndexItem(((int)(i)), tr.get());
                                        _set.value = i;
                                        final int r = FutureEnd.this._counter.decrementAndGet();
                                        if(FutureEnd.this._ended && r == 0) {
                                            if(!(FutureEnd.this._yielded.getAndSet(true))) {
                                                if(FutureEnd.this._array == null) {
                                                    throw new NullPointerException();
                                                }
                                                FutureEnd.this._promise.successValue(FutureEnd.this._array.im());
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    });
                }
                if(FutureEnd.this._stopped) {
                    return Go.Break;
                } else {
                    return Go.Continue;
                }
            }
        }, new F<Go, Go>() {
            @Override
            public Go apply(final Go res) {
                FutureEnd.this._ended = true;
                if(FutureEnd.this._counter.get() == 0) {
                    if(!(FutureEnd.this._yielded.getAndSet(true))) {
                        if(FutureEnd.this._array == null) {
                            throw new NullPointerException();
                        }
                        FutureEnd.this._promise.successValue(FutureEnd.this._array.im());
                    }
                }
                return res;
            }
        });
    }
    public FutureEnd() {
        this._promise = Promise.<ImArray<T>>apply();
        this._stopped = false;
        this._counter = new AtomicInt();
        this._ended = false;
        this._yielded = new AtomicBool();
    }
    public String toString() {
        return "FutureEnd";
    }
}