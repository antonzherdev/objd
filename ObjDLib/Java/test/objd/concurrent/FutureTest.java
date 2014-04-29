package objd.concurrent;

import test.;
import test.Test;
import test.TestCase;
import test..*;

public class FutureTest extends TestCase {
    public void testPromiseOnComplete() {
        final AtomicInt n = new AtomicInt();
        final int count = 100000;
        {
            final Iterator<Integer> __inline__2_i = 1.to(count).iterator();
            while(__inline__2_i.hasNext()) {
                final Integer i = __inline__2_i.next();
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
        Thread.sleepPeriod(((float)1));
        .<Integer>assertEqualsAB(n.intValue(), ((int)count));
    }
    public void testMap() {
        final AtomicInt n = new AtomicInt();
        final int count = 100;
        AtomicInt result = new AtomicInt();
        {
            final Iterator<Integer> __inline__3_i = 1.to(count).iterator();
            while(__inline__3_i.hasNext()) {
                final Integer __inline__3_v = __inline__3_i.next();
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
                        result.addAndGetValue(((int)__inline__3_v + 1));
                        DispatchQueue.aDefault.asyncF(new P0() {
                            @Override
                            public void apply() {
                                p.successValue(__inline__3_v);
                            }
                        });
                        m.onCompleteF(new P<Try<Integer>>() {
                            @Override
                            public void apply(final Try<Integer> _) {
                                n.addAndGetValue(((int)_.get()));
                            }
                        });
                    }
                });
            }
        }
        Thread.sleepPeriod(((float)3));
        .<AtomicInt>assertEqualsAB(n, result);
    }
    public void testFlatMap() {
        final AtomicInt n = new AtomicInt();
        final int count = 100;
        int result = 0;
        {
            final Iterator<Integer> __inline__3_i = 1.to(count).iterator();
            while(__inline__3_i.hasNext()) {
                final Integer i = __inline__3_i.next();
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
                            n.addAndGetValue(((int)_.get()));
                        }
                    });
                }
            }
        }
        Thread.sleepPeriod(((float)3));
        .<Integer>assertEqualsAB(n.intValue(), ((int)result));
    }
    public FutureTest() {
    }
}