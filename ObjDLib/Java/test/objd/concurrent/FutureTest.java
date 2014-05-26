package objd.concurrent;

import objd.lang.*;
import objd.collection.Iterator;
import test.;
import test.TestCase;
import test.Test;
import test..*;

public class FutureTest extends TestCase {
    public void testPromiseOnComplete() {
        final AtomicInt n = new AtomicInt();
        final int count = 100000;
        {
            final Iterator<Integer> __il__2i = Int.to(1, count).iterator();
            while(__il__2i.hasNext()) {
                final Integer i = __il__2i.next();
                {
                    final Promise<Integer> p = Promise.<Integer>apply();
                    DispatchQueue.aDefault.asyncF(new P0() {
                        @Override
                        public void apply() {
                            p.successValue(i);
                        }
                    });
                    p.onCompleteF(new P<Try<Integer>>() {
                        @Override
                        public void apply(final Try<Integer> _) {
                            n.incrementAndGet();
                        }
                    });
                }
            }
        }
        Thread.sleepPeriod(((double)(1)));
        .<Integer>assertEqualsAB(n.get(), ((int)(count)));
    }
    public void testMap() {
        final AtomicInt n = new AtomicInt();
        final int count = 100;
        AtomicInt result = new AtomicInt();
        {
            final Iterator<Integer> __il__3i = Int.to(1, count).iterator();
            while(__il__3i.hasNext()) {
                final Integer __il__3v = __il__3i.next();
                DispatchQueue.aDefault.asyncF(new P0() {
                    @Override
                    public void apply() {
                        final Promise<Integer> p = Promise.<Integer>apply();
                        final Future<Integer> m = p.<Integer>mapF(new F<Integer, Integer>() {
                            @Override
                            public Integer apply(final Integer _) {
                                return _ + 1;
                            }
                        });
                        result.addAndGet(((int)(__il__3v + 1)));
                        DispatchQueue.aDefault.asyncF(new P0() {
                            @Override
                            public void apply() {
                                p.successValue(__il__3v);
                            }
                        });
                        m.onCompleteF(new P<Try<Integer>>() {
                            @Override
                            public void apply(final Try<Integer> _) {
                                n.addAndGet(((int)(_.get())));
                            }
                        });
                    }
                });
            }
        }
        Thread.sleepPeriod(((double)(3)));
        .<AtomicInt>assertEqualsAB(n, result);
    }
    public void testFlatMap() {
        final AtomicInt n = new AtomicInt();
        final int count = 100;
        int result = 0;
        {
            final Iterator<Integer> __il__3i = Int.to(1, count).iterator();
            while(__il__3i.hasNext()) {
                final Integer i = __il__3i.next();
                {
                    final Promise<Integer> p = Promise.<Integer>apply();
                    final Future<Integer> m = p.<Integer>flatMapF(new F<Integer, Future<Integer>>() {
                        @Override
                        public Future<Integer> apply(final Integer _) {
                            return Future.<Integer>applyF(new F0<Integer>() {
                                @Override
                                public Integer apply() {
                                    return _ + 1;
                                }
                            });
                        }
                    });
                    result += i + 1;
                    DispatchQueue.aDefault.asyncF(new P0() {
                        @Override
                        public void apply() {
                            p.successValue(i);
                        }
                    });
                    m.onCompleteF(new P<Try<Integer>>() {
                        @Override
                        public void apply(final Try<Integer> _) {
                            n.addAndGet(((int)(_.get())));
                        }
                    });
                }
            }
        }
        Thread.sleepPeriod(((double)(3)));
        .<Integer>assertEqualsAB(n.get(), ((int)(result)));
    }
    public FutureTest() {
    }
}