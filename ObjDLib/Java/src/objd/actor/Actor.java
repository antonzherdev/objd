package objd.actor;

import objd.lang.*;
import objd.concurrent.Future;
import objd.concurrent.Promise;

public class Actor {
    public final Mailbox mailbox;
    public <T> Future<T> futureF(final F0<T> f) {
        final ActorFuture<T> fut = new ActorFuture<T>(this, false, f);
        this.mailbox.sendMessage(((ActorMessage)(fut)));
        return fut;
    }
    public <T> Future<T> promptF(final F0<T> f) {
        final ActorFuture<T> fut = new ActorFuture<T>(this, true, f);
        this.mailbox.sendMessage(((ActorMessage)(fut)));
        return fut;
    }
    public <T> Future<T> futureJoinF(final F0<Future<T>> f) {
        final Promise<T> ret = Promise.<T>apply();
        final ActorFuture<Void> fut = new ActorFuture<Void>(this, false, new F0<Void>() {
            @Override
            public Void apply() {
                {
                    final Future<T> nf = f.apply();
                    nf.onCompleteF(new P<Try<T>>() {
                        @Override
                        public void apply(final Try<T> _) {
                            ret.completeValue(_);
                        }
                    });
                }
                return null;
            }
        });
        this.mailbox.sendMessage(((ActorMessage)(fut)));
        return ret;
    }
    public <T> Future<T> promptJoinF(final F0<Future<T>> f) {
        final Promise<T> ret = Promise.<T>apply();
        final ActorFuture<Void> fut = new ActorFuture<Void>(this, true, new F0<Void>() {
            @Override
            public Void apply() {
                {
                    final Future<T> nf = f.apply();
                    nf.onCompleteF(new P<Try<T>>() {
                        @Override
                        public void apply(final Try<T> _) {
                            ret.completeValue(_);
                        }
                    });
                }
                return null;
            }
        });
        this.mailbox.sendMessage(((ActorMessage)(fut)));
        return ret;
    }
    public <T, R> Future<R> onSuccessFutureF(final Future<T> future, final F<T, R> f) {
        final Mut<T> res = new Mut<T>();
        final ActorFuture<R> fut = new ActorFuture<R>(this, false, new F0<R>() {
            @Override
            public R apply() {
                if(res.value == null) {
                    throw new NullPointerException();
                }
                return f.apply(res.value);
            }
        });
        future.onCompleteF(new P<Try<T>>() {
            @Override
            public void apply(final Try<T> tr) {
                if(tr.isFailure()) {
                    fut.completeValue(((Try<R>)(((Try)(tr)))));
                } else {
                    res.value = tr.get();
                    Actor.this.mailbox.sendMessage(((ActorMessage)(fut)));
                }
            }
        });
        return fut;
    }
    public <T, R> Future<R> lockAndOnSuccessFutureF(final Future<T> future, final F<T, R> f) {
        final Mut<T> res = new Mut<T>();
        final ActorFuture<R> fut = new ActorFuture<R>(this, false, new F0<R>() {
            @Override
            public R apply() {
                if(res.value == null) {
                    throw new NullPointerException();
                }
                return f.apply(res.value);
            }
        });
        fut.lock();
        future.onCompleteF(new P<Try<T>>() {
            @Override
            public void apply(final Try<T> tr) {
                if(tr.isFailure()) {
                    fut.completeValue(((Try<R>)(((Try)(tr)))));
                } else {
                    res.value = tr.get();
                    fut.unlock();
                }
            }
        });
        this.mailbox.sendMessage(((ActorMessage)(fut)));
        return fut;
    }
    public Future<Void> dummy() {
        return this.<Void>futureF(new F0<Void>() {
            @Override
            public Void apply() {
                return null;
            }
        });
    }
    public Actor() {
        this.mailbox = new Mailbox();
    }
    public String toString() {
        return "Actor";
    }
}