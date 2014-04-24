package core.chain;

import test.;
import test.Test;
import test.TestCase;
import test..*;

public class FutureTest extends TestCase {
    public void testPromiseOnComplete() {
        AtomicInt n = new AtomicInt();
        int count = 100000;
        {
            Iterator<Integer> __inline__2_i = 1.to(count).iterator();
            while(__inline__2_i.hasNext()) {
                Integer i = __inline__2_i.next();
                {
                    Promise<Integer> p = Promise().apply<Integer>();
                    DispatchQueue().default.async(new P0() {
                        @Override
                        public void apply() {
                            return p.successValue(i);
                        }
                    });
                    p.onCompleteF(new P<Try<Integer>>() {
                        @Override
                        public void apply(Try<Integer> _) {
                            n.incrementAndGet();
                        }
                    });
                }
            }
        }
        Thread().sleep(((float)1));
        ().assertEquals<Integer>(n.intValue(), ((int)count));
    }
    public void testMap() {
        AtomicInt n = new AtomicInt();
        int count = 100;
        AtomicInt result = new AtomicInt();
        {
            Iterator<Integer> __inline__3_i = 1.to(count).iterator();
            while(__inline__3_i.hasNext()) {
                Integer __inline__3_v = __inline__3_i.next();
                DispatchQueue().default.async(new P0() {
                    @Override
                    public void apply() {
                        Promise<Integer> p = Promise().apply<Integer>();
                        Future<Integer> m = p.mapF<Integer>(new F<Integer, Integer>() {
                            @Override
                            public Integer apply(Integer _) {
                                return _ + 1;
                            }
                        });
                        result.addAndGet(((int)__inline__3_v + 1));
                        DispatchQueue().default.async(new P0() {
                            @Override
                            public void apply() {
                                return p.successValue(__inline__3_v);
                            }
                        });
                        m.onCompleteF(new P<Try<Integer>>() {
                            @Override
                            public void apply(Try<Integer> _) {
                                n.addAndGet(((int)_.get()));
                            }
                        });
                    }
                });
            }
        }
        Thread().sleep(((float)3));
        ().assertEquals<AtomicInt>(n, result);
    }
    public void testFlatMap() {
        AtomicInt n = new AtomicInt();
        int count = 100;
        int result = 0;
        {
            Iterator<Integer> __inline__3_i = 1.to(count).iterator();
            while(__inline__3_i.hasNext()) {
                Integer i = __inline__3_i.next();
                {
                    Promise<Integer> p = Promise().apply<Integer>();
                    Future<Integer> m = p.flatMapF<Integer>(new F<Integer, Future<Integer>>() {
                        @Override
                        public Future<Integer> apply(Integer _) {
                            return Future().applyF<Integer>(new F0<Integer>() {
                                @Override
                                public Integer apply() {
                                    return _ + 1;
                                }
                            });
                        }
                    });
                    result += i + 1;
                    DispatchQueue().default.async(new P0() {
                        @Override
                        public void apply() {
                            return p.successValue(i);
                        }
                    });
                    m.onCompleteF(new P<Try<Integer>>() {
                        @Override
                        public void apply(Try<Integer> _) {
                            n.addAndGet(((int)_.get()));
                        }
                    });
                }
            }
        }
        Thread().sleep(((float)3));
        ().assertEquals<Integer>(n.intValue(), ((int)result));
    }
    public FutureTest() {
    }
}