package core.chain;

import test.;
import test.Test;
import test.TestCase;
import test..*;

public class ChainTest extends TestCase {
    public void testAnd() {
        ().assertTrue(ERROR: Unknown !([True, False, True].<rdI>chain\Chain#C<§^bool§>\.<dIub>and\bool\));
        ().assertTrue(ERROR: Unknown !([False, False, False].<rdI>chain\Chain#C<§^bool§>\.<dIub>and\bool\));
        ().assertTrue(ImArray.fromObjects(true, true, true).chain().and());
        ().assertTrue(ImArray.fromObjects().chain().and());
    }
    public void testOr() {
        ().assertTrue(ImArray.fromObjects(false, false, true).chain().or());
        ().assertTrue(ERROR: Unknown !([False, False, False].<rdI>chain\Chain#C<§^bool§>\.<dIub>or\bool\));
        ().assertTrue(ImArray.fromObjects(true, true, true).chain().or());
        ().assertTrue(ERROR: Unknown !([].<rdI>chain\Chain#C<§^_unset§>\.<dIub>or\bool\));
    }
    public void testFuture() {
        repeat(((int)1000), new P0() {
            @Override
            public void apply() {
                ImArray<Tuple2<Integer, Promise<Integer>>> arr = 0.to(1000).chain().map<Tuple2<Integer, Promise<Integer>>>(new F<Integer, Tuple2<Integer, Promise<Integer>>>() {
                    @Override
                    public Tuple2<Integer, Promise<Integer>> apply(Integer i) {
                        return ERROR: Unknown (<l>i\§^int§\, <to>Promise\Promise#C.class\.<dIt>apply\Promise#C<§^int§>\);
                    }
                }).toArray();
                {
                    Iterator<Tuple2<Integer, Promise<Integer>>> __inline__0_1_i = arr.iterator();
                    while(__inline__0_1_i.hasNext()) {
                        Tuple2<Integer, Promise<Integer>> t = __inline__0_1_i.next();
                        DispatchQueue().default.async(new P0() {
                            @Override
                            public void apply() {
                                return t.b.successValue(t.a * t.a);
                            }
                        });
                    }
                }
                Future<ImArray<Integer>> fut = arr.chain().map<Promise<Integer>>(new F<Tuple2<Integer, Promise<Integer>>, Promise<Integer>>() {
                    @Override
                    public Promise<Integer> apply(Tuple2<Integer, Promise<Integer>> _) {
                        return _.b;
                    }
                }).future<Integer, ImArray<Integer>>(new F<Chain<Integer>, ImArray<Integer>>() {
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
                ().assertEquals<ImArray<Integer>>(set, fut.getResultAwait(((float)5)));
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
        {
            Iterator<Promise<Void>> __inline__3_i = arr.iterator();
            while(__inline__3_i.hasNext()) {
                Promise<Void> p = __inline__3_i.next();
                DispatchQueue().default.async(new P0() {
                    @Override
                    public void apply() {
                        count.incrementAndGet();
                        return p.successValue(null);
                    }
                });
            }
        }
        ().assertTrue(fut.waitResultPeriod(((float)5)) != null);
        ().assertEquals<Integer>(count.intValue(), ((int)arr.count()));
    }
    public void testFlat() {
        ().assertEquals<ImArray<Integer>>(ImArray.fromObjects(1, 5, 2, 3, 2), ImArray.fromObjects(((ImArray<Integer>)ImArray.fromObjects(1, 5)), ((ImArray<Integer>)ImArray.fromObjects(2, 3)), ImArray.fromObjects(2)).chain().flat<Integer>().toArray());
    }
    public void testZip() {
        ().assertEquals<ImArray<Integer>>(ImArray.fromObjects(2, 3), ImArray.fromObjects(1, 0, 3).chain().zip<Integer, Integer>(ImArray.fromObjects(1, 3), new F2<Integer, Integer, Integer>() {
            @Override
            public Integer apply(Integer a,Integer b) {
                return a + b;
            }
        }).toArray());
    }
    public void testZip3() {
        ().assertEquals<ImArray<Integer>>(ImArray.fromObjects(3, 4), ImArray.fromObjects(1, 0, 3).chain().zip3<Integer, Integer, Integer>(ImArray.fromObjects(1, 3), ImArray.fromObjects(1, 1, 2, 4), new F3<Integer, Integer, Integer, Integer>() {
            @Override
            public Integer apply(Integer a,Integer b,Integer c) {
                return a + b + c;
            }
        }).toArray());
    }
    public void testZipFor() {
        ImArray<Integer> arr = ImArray.fromObjects();
        ImArray.fromObjects(1, 0, 3).chain().zipFor<Integer>(ImArray.fromObjects(1, 3), new P2<Integer, Integer>() {
            @Override
            public void apply(Integer a,Integer b) {
                arr = arr.add(a + b);
            }
        });
        ().assertEquals<ImArray<Integer>>(ImArray.fromObjects(2, 3), arr);
    }
    public ChainTest() {
    }
}