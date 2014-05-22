package objd.chain;

import objd.lang.*;
import objd.concurrent.Promise;
import objd.concurrent.AtomicInt;
import objd.concurrent.AtomicBool;
import objd.concurrent.Future;
import objd.collection.Go;

public class FutureVoidEnd {
    private final Promise<Void> _promise;
    private boolean _stopped;
    private AtomicInt _counter;
    private volatile boolean _ended;
    private AtomicBool _yielded;
    public Future<Void> future() {
        return this._promise;
    }
    public Yield<Future<Void>> yield() {
        return Yield.<Future<Void>>makeBeginYieldEnd(new F<Integer, Go>() {
            @Override
            public Go apply(final Integer size) {
                return Go.Continue;
            }
        }, new F<Future<Void>, Go>() {
            @Override
            public Go apply(final Future<Void> fut) {
                if(!(FutureVoidEnd.this._stopped)) {
                    FutureVoidEnd.this._counter.incrementAndGet();
                    fut.onCompleteF(new P<Try<Void>>() {
                        @Override
                        public void apply(final Try<Void> tr) {
                            if(!(FutureVoidEnd.this._stopped)) {
                                if(tr.isFailure()) {
                                    FutureVoidEnd.this._stopped = true;
                                    FutureVoidEnd.this._promise.failureReason(tr);
                                } else {
                                    if(!(FutureVoidEnd.this._stopped)) {
                                        final int r = FutureVoidEnd.this._counter.decrementAndGet();
                                        if(FutureVoidEnd.this._ended && r == 0) {
                                            if(!(FutureVoidEnd.this._yielded.getAndSet(true))) {
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
                    return Go.Break;
                } else {
                    return Go.Continue;
                }
            }
        }, new F<Go, Go>() {
            @Override
            public Go apply(final Go res) {
                Go ret = res;
                FutureVoidEnd.this._ended = true;
                if(FutureVoidEnd.this._counter.intValue() == 0) {
                    if(!(FutureVoidEnd.this._yielded.getAndSet(true))) {
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