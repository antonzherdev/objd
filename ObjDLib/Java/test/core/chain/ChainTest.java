package core.chain;

import test.;
import test.Test;
import test.TestCase;
import test..*;

public class ChainTest extends TestCase {
    public void testAnd() {
        .assertTrueValue(!(ImArray.fromObjects(true, false, true).chain().and()));
        .assertTrueValue(!(ImArray.fromObjects(false, false, false).chain().and()));
        .assertTrueValue(ImArray.fromObjects(true, true, true).chain().and());
        .assertTrueValue(ImArray.fromObjects().chain().and());
    }
    public void testOr() {
        .assertTrueValue(ImArray.fromObjects(false, false, true).chain().or());
        .assertTrueValue(!(ImArray.fromObjects(false, false, false).chain().or()));
        .assertTrueValue(ImArray.fromObjects(true, true, true).chain().or());
        .assertTrueValue(!(ImArray.fromObjects().chain().or()));
    }
    public void testFuture() {
        repeatTimesF(((int)1000), new P0() {
            @Override
            public void apply() {
                final ImArray<Tuple<Integer, Promise<Integer>>> arr = 0.to(1000).chain().map<Tuple<Integer, Promise<Integer>>>(new F<Integer, Tuple<Integer, Promise<Integer>>>() {
                    @Override
                    public Tuple<Integer, Promise<Integer>> apply(final Integer i) {
                        return new Tuple<Integer, Promise<Integer>>(i, Promise.<Integer>apply());
                    }
                }).toArray();
                {
                    final Iterator<Tuple<Integer, Promise<Integer>>> __inline__0_1_i = arr.iterator();
                    while(__inline__0_1_i.hasNext()) {
                        final Tuple<Integer, Promise<Integer>> t = __inline__0_1_i.next();
                        DispatchQueue.default.asyncF(new P0() {
                            @Override
                            public void apply() {
                                return t.b.successValue(t.a * t.a);
                            }
                        });
                    }
                }
                final Future<ImArray<Integer>> fut = arr.chain().map<Promise<Integer>>(new F<Tuple<Integer, Promise<Integer>>, Promise<Integer>>() {
                    @Override
                    public Promise<Integer> apply(final Tuple<Integer, Promise<Integer>> _) {
                        return _.b;
                    }
                }).futureF<Integer, ImArray<Integer>>(new F<Chain<Integer>, ImArray<Integer>>() {
                    @Override
                    public ImArray<Integer> apply(final Chain<Integer> chain) {
                        return chain.toArray();
                    }
                });
                final ImArray<Integer> set = arr.chain().map<Integer>(new F<Tuple<Integer, Promise<Integer>>, Integer>() {
                    @Override
                    public Integer apply(final Tuple<Integer, Promise<Integer>> _) {
                        return _.a;
                    }
                }).map<Integer>(new F<Integer, Integer>() {
                    @Override
                    public Integer apply(final Integer _) {
                        return _ * _;
                    }
                }).toArray();
                .<ImArray<Integer>>assertEqualsAB(set, fut.getResultAwait(((float)5)));
            }
        });
    }
    public void testVoidFuture() {
        final ImArray<Promise<Void>> arr = 0.to(1000).chain().map<Promise<Void>>(new F<Integer, Promise<Void>>() {
            @Override
            public Promise<Void> apply(final Integer i) {
                return Promise.<Void>apply();
            }
        }).toArray();
        final Future<Void> fut = arr.chain().voidFuture();
        AtomicInt count = new AtomicInt();
        {
            final Iterator<Promise<Void>> __inline__3_i = arr.iterator();
            while(__inline__3_i.hasNext()) {
                final Promise<Void> p = __inline__3_i.next();
                DispatchQueue.default.asyncF(new P0() {
                    @Override
                    public void apply() {
                        count.incrementAndGet();
                        return p.successValue(null);
                    }
                });
            }
        }
        .assertTrueValue(fut.waitResultPeriod(((float)5)) != null);
        .<Integer>assertEqualsAB(count.intValue(), ((int)arr.count()));
    }
    public void testFlat() {
        .<ImArray<Integer>>assertEqualsAB(ImArray.fromObjects(1, 5, 2, 3, 2), ImArray.fromObjects(((ImArray<Integer>)ImArray.fromObjects(1, 5)), ((ImArray<Integer>)ImArray.fromObjects(2, 3)), ImArray.fromObjects(2)).chain().flat<Integer>().toArray());
    }
    public void testZip() {
        .<ImArray<Integer>>assertEqualsAB(ImArray.fromObjects(2, 3), ImArray.fromObjects(1, 0, 3).chain().zipABy<Integer, Integer>(ImArray.fromObjects(1, 3), new F2<Integer, Integer, Integer>() {
            @Override
            public Integer apply(final Integer a, final Integer b) {
                return a + b;
            }
        }).toArray());
    }
    public void testZip3() {
        .<ImArray<Integer>>assertEqualsAB(ImArray.fromObjects(3, 4), ImArray.fromObjects(1, 0, 3).chain().zip3ABBy<Integer, Integer, Integer>(ImArray.fromObjects(1, 3), ImArray.fromObjects(1, 1, 2, 4), new F3<Integer, Integer, Integer, Integer>() {
            @Override
            public Integer apply(final Integer a, final Integer b, final Integer c) {
                return a + b + c;
            }
        }).toArray());
    }
    public void testZipFor() {
        ImArray<Integer> arr = ImArray.fromObjects();
        ImArray.fromObjects(1, 0, 3).chain().zipForABy<Integer>(ImArray.fromObjects(1, 3), new P2<Integer, Integer>() {
            @Override
            public void apply(final Integer a, final Integer b) {
                arr = arr.addItem(a + b);
            }
        });
        .<ImArray<Integer>>assertEqualsAB(ImArray.fromObjects(2, 3), arr);
    }
    public ChainTest() {
    }
}