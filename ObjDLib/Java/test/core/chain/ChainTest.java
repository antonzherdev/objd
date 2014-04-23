package core.chain;

import test.;
import test.Test;
import test.TestCase;
import test..*;

public class ChainTest extends TestCase {
    public void testAnd() {
        ().assertTrueValue(ERROR: Unknown !([True, False, True].<rdI>chain\Chain#C<§^bool§>\.<dIub>and\bool\));
        ().assertTrueValue(ERROR: Unknown !([False, False, False].<rdI>chain\Chain#C<§^bool§>\.<dIub>and\bool\));
        ().assertTrueValue(ImArray.fromObjects(true, true, true).chain().and());
        ().assertTrueValue(ImArray.fromObjects().chain().and());
    }
    public void testOr() {
        ().assertTrueValue(ImArray.fromObjects(false, false, true).chain().or());
        ().assertTrueValue(ERROR: Unknown !([False, False, False].<rdI>chain\Chain#C<§^bool§>\.<dIub>or\bool\));
        ().assertTrueValue(ImArray.fromObjects(true, true, true).chain().or());
        ().assertTrueValue(ERROR: Unknown !([].<rdI>chain\Chain#C<§^_unset§>\.<dIub>or\bool\));
    }
    public void testFuture() {
        repeatTimesF(ERROR: Unknown 1000.cast<uint>, new P0() {
            @Override
            public void apply() {
                ImArray<Tuple2<Integer, Promise<Integer>>> arr = 0.to(1000).chain().map<Tuple2<Integer, Promise<Integer>>>(new F<Integer, Tuple2<Integer, Promise<Integer>>>() {
                    @Override
                    public Tuple2<Integer, Promise<Integer>> apply(Integer i) {
                        return ERROR: Unknown (<l>i\§^int§\, <to>Promise\Promise#C.class\.<dIt>apply\Promise#C<§^int§>\);
                    }
                }).toArray();
                arr.forEach(new P<Tuple2<Integer, Promise<Integer>>>() {
                    @Override
                    public void apply(Tuple2<Integer, Promise<Integer>> t) {
                        DispatchQueue().default.asyncF(new P0() {
                            @Override
                            public void apply() {
                                return t.b.successValue(t.a * t.a);
                            }
                        });
                    }
                });
                Future<ImArray<Integer>> fut = arr.chain().map<Promise<Integer>>(new F<Tuple2<Integer, Promise<Integer>>, Promise<Integer>>() {
                    @Override
                    public Promise<Integer> apply(Tuple2<Integer, Promise<Integer>> _) {
                        return _.b;
                    }
                }).futureF<Integer, ImArray<Integer>>(new F<Chain<Integer>, ImArray<Integer>>() {
                    @Override
                    public ImArray<Integer> apply(Chain<Integer> chain) {
                        return chain.toArray();
                    }
                });
                ImArray<Integer> set = arr.chain().map<Integer>(new F<Tuple2<Integer, Promise<Integer>>, Integer>() {
                    @Override
                    public Integer apply(Tuple2<Integer, Promise<Integer>> _) {
                        return _.a;
                    }
                }).map<Integer>(new F<Integer, Integer>() {
                    @Override
                    public Integer apply(Integer _) {
                        return _ * _;
                    }
                }).toArray();
                ().assertEqualsAB<ImArray<Integer>>(set, fut.getResultAwait(ERROR: Unknown 5.cast<float>));
            }
        });
    }
    public void testVoidFuture() {
        ImArray<Promise<Void>> arr = 0.to(1000).chain().map<Promise<Void>>(new F<Integer, Promise<Void>>() {
            @Override
            public Promise<Void> apply(Integer i) {
                return Promise().apply<Void>();
            }
        }).toArray();
        Future<Void> fut = arr.chain().voidFuture();
        AtomicInt count = new AtomicInt();
        arr.forEach(new P<Promise<Void>>() {
            @Override
            public void apply(Promise<Void> p) {
                DispatchQueue().default.asyncF(new P0() {
                    @Override
                    public void apply() {
                        count.incrementAndGet();
                        return p.successValue(null);
                    }
                });
            }
        });
        ().assertTrueValue(fut.waitResultPeriod(ERROR: Unknown 5.cast<float>) != null);
        ().assertEqualsAB<Integer>(count.intValue(), ERROR: Unknown <l>arr\[^Promise#C<^void>]\.<rdI>count\uint\.cast<int4>);
    }
    public void testFlat() {
        ().assertEqualsAB<ImArray<Integer>>(ImArray.fromObjects(1, 5, 2, 3, 2), ImArray.fromObjects(ERROR: Unknown [1, 5].cast<^int[1]>, ERROR: Unknown [2, 3].cast<^int[1]>, ImArray.fromObjects(2)).chain().flat<Integer>().toArray());
    }
    public void testZip() {
        ().assertEqualsAB<ImArray<Integer>>(ImArray.fromObjects(2, 3), ImArray.fromObjects(1, 0, 3).chain().zipABy<Integer, Integer>(ImArray.fromObjects(1, 3), new F2<Integer, Integer, Integer>() {
            @Override
            public Integer apply(Integer a,Integer b) {
                return a + b;
            }
        }).toArray());
    }
    public void testZip3() {
        ().assertEqualsAB<ImArray<Integer>>(ImArray.fromObjects(3, 4), ImArray.fromObjects(1, 0, 3).chain().zip3ABBy<Integer, Integer, Integer>(ImArray.fromObjects(1, 3), ImArray.fromObjects(1, 1, 2, 4), new F3<Integer, Integer, Integer, Integer>() {
            @Override
            public Integer apply(Integer a,Integer b,Integer c) {
                return a + b + c;
            }
        }).toArray());
    }
    public void testZipFor() {
        ImArray<Integer> arr = ImArray.fromObjects();
        ImArray.fromObjects(1, 0, 3).chain().zipForABy<Integer>(ImArray.fromObjects(1, 3), new P2<Integer, Integer>() {
            @Override
            public void apply(Integer a,Integer b) {
                arr = arr.addItem(a + b);
            }
        });
        ().assertEqualsAB<ImArray<Integer>>(ImArray.fromObjects(2, 3), arr);
    }
    public ChainTest() {
    }
}