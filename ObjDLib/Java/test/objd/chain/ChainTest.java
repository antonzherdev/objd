package objd.chain;

import objd.lang.*;
import test.PackageObjectTest;
import objd.collection.ImArray;
import objd.concurrent.Promise;
import objd.concurrent.DispatchQueue;
import objd.concurrent.Future;
import objd.concurrent.AtomicInt;
import objd.collection.Traversable;
import objd.collection.Set;
import test.TestCase;
import test.Test;
import test.PackageObjectTest.*;

public class ChainTest extends TestCase {
    public void testAnd() {
        PackageObjectTest.assertTrueValue(!(ImArray.fromObjects(true, false, true).chain().and()));
        PackageObjectTest.assertTrueValue(!(ImArray.fromObjects(false, false, false).chain().and()));
        PackageObjectTest.assertTrueValue(ImArray.fromObjects(true, true, true).chain().and());
        PackageObjectTest.assertTrueValue(ImArray.fromObjects().chain().and());
    }
    public void testOr() {
        PackageObjectTest.assertTrueValue(ImArray.fromObjects(false, false, true).chain().or());
        PackageObjectTest.assertTrueValue(!(ImArray.fromObjects(false, false, false).chain().or()));
        PackageObjectTest.assertTrueValue(ImArray.fromObjects(true, true, true).chain().or());
        PackageObjectTest.assertTrueValue(!(ImArray.fromObjects().chain().or()));
    }
    public void testFuture() {
        repeatTimesF(((int)(1000)), new P0() {
            @Override
            public void apply() {
                final ImArray<Tuple<Integer, Promise<Integer>>> arr = ((ImArray<Tuple<Integer, Promise<Integer>>>)(((ImArray)(Int.to(0, 1000).chain().<Tuple<Integer, Promise<Integer>>>mapF(new F<Integer, Tuple<Integer, Promise<Integer>>>() {
                    @Override
                    public Tuple<Integer, Promise<Integer>> apply(final Integer i) {
                        return new Tuple<Integer, Promise<Integer>>(i, Promise.<Integer>apply());
                    }
                }).toArray()))));
                arr.forEach(new P<Tuple<Integer, Promise<Integer>>>() {
                    @Override
                    public void apply(final Tuple<Integer, Promise<Integer>> t) {
                        DispatchQueue.aDefault.asyncF(new P0() {
                            @Override
                            public void apply() {
                                t.b.successValue(t.a * t.a);
                            }
                        });
                    }
                });
                final Future<ImArray<Integer>> fut = ((Future<ImArray<Integer>>)(((Future)(arr.chain().<Promise<Integer>>mapF(new F<Tuple<Integer, Promise<Integer>>, Promise<Integer>>() {
                    @Override
                    public Promise<Integer> apply(final Tuple<Integer, Promise<Integer>> _) {
                        return _.b;
                    }
                }).<Integer, ImArray<Integer>>futureF(new F<Chain<Integer>, ImArray<Integer>>() {
                    @Override
                    public ImArray<Integer> apply(final Chain<Integer> chain) {
                        return chain.toArray();
                    }
                })))));
                final ImArray<Integer> set = arr.chain().<Integer>mapF(new F<Tuple<Integer, Promise<Integer>>, Integer>() {
                    @Override
                    public Integer apply(final Tuple<Integer, Promise<Integer>> _) {
                        return _.a;
                    }
                }).<Integer>mapF(new F<Integer, Integer>() {
                    @Override
                    public Integer apply(final Integer _) {
                        return _ * _;
                    }
                }).toArray();
                PackageObjectTest.<ImArray<Integer>>assertEqualsAB(set, fut.getResultAwait(((double)(5))));
            }
        });
    }
    public void testVoidFuture() {
        final ImArray<Promise<Void>> arr = Int.to(0, 1000).chain().<Promise<Void>>mapF(new F<Integer, Promise<Void>>() {
            @Override
            public Promise<Void> apply(final Integer i) {
                return Promise.<Void>apply();
            }
        }).toArray();
        final Future<Void> fut = arr.chain().voidFuture();
        final AtomicInt count = new AtomicInt();
        arr.forEach(new P<Promise<Void>>() {
            @Override
            public void apply(final Promise<Void> p) {
                DispatchQueue.aDefault.asyncF(new P0() {
                    @Override
                    public void apply() {
                        count.incrementAndGet();
                        p.successValue(null);
                    }
                });
            }
        });
        PackageObjectTest.assertTrueValue(fut.waitResultPeriod(((double)(5))) != null);
        PackageObjectTest.<Integer>assertEqualsAB(count.get(), ((int)(arr.count())));
    }
    public void testMap() {
        PackageObjectTest.<ImArray<Integer>>assertEqualsAB(ImArray.fromObjects(4, 0, 2), ImArray.fromObjects(2, 0, 1).chain().<Integer>mapF(new F<Integer, Integer>() {
            @Override
            public Integer apply(final Integer x) {
                return 2 * x;
            }
        }).toArray());
    }
    public void testMapOpt() {
        PackageObjectTest.<ImArray<Integer>>assertEqualsAB(ImArray.fromObjects(4, 2), ImArray.fromObjects(2, 0, 1).chain().<Integer>mapOptF(((F<Integer, Integer>)(((F)(new F<Integer, Integer>() {
            @Override
            public Integer apply(final Integer x) {
                if(x == 0) {
                    return null;
                } else {
                    return 2 * x;
                }
            }
        }))))).toArray());
    }
    public void testFlatMap() {
        PackageObjectTest.<ImArray<Integer>>assertEqualsAB(ImArray.fromObjects(2, 4, 0, 0, 1, 2), ImArray.fromObjects(2, 0, 1).chain().<Integer>flatMapF(((F<Integer, Traversable<Integer>>)(((F)(new F<Integer, ImArray<Integer>>() {
            @Override
            public ImArray<Integer> apply(final Integer x) {
                return ImArray.fromObjects(x, 2 * x);
            }
        }))))).toArray());
    }
    public void testFlat() {
        PackageObjectTest.<ImArray<Integer>>assertEqualsAB(ImArray.fromObjects(1, 5, 2, 3, 2), ImArray.fromObjects(((ImArray<Integer>)(((ImArray)(ImArray.fromObjects(1, 5))))), ((ImArray<Integer>)(((ImArray)(ImArray.fromObjects(2, 3))))), ImArray.fromObjects(2)).chain().<Integer>flat().toArray());
    }
    public void testZip() {
        PackageObjectTest.<ImArray<Integer>>assertEqualsAB(ImArray.fromObjects(2, 3), ImArray.fromObjects(1, 0, 3).chain().<Integer, Integer>zipBBy(ImArray.fromObjects(1, 3), new F2<Integer, Integer, Integer>() {
            @Override
            public Integer apply(final Integer a, final Integer b) {
                return a + b;
            }
        }).toArray());
    }
    public void testZip3() {
        PackageObjectTest.<ImArray<Integer>>assertEqualsAB(ImArray.fromObjects(3, 4), ImArray.fromObjects(1, 0, 3).chain().<Integer, Integer, Integer>zip3BCBy(ImArray.fromObjects(1, 3), ImArray.fromObjects(1, 1, 2, 4), new F3<Integer, Integer, Integer, Integer>() {
            @Override
            public Integer apply(final Integer a, final Integer b, final Integer c) {
                return a + b + c;
            }
        }).toArray());
    }
    public void testZipFor() {
        final Mut<ImArray<Integer>> arr = new Mut<ImArray<Integer>>(ImArray.<Integer>empty());
        ImArray.fromObjects(1, 0, 3).chain().<Integer>zipForBBy(ImArray.fromObjects(1, 3), new P2<Integer, Integer>() {
            @Override
            public void apply(final Integer a, final Integer b) {
                arr.value = arr.value.addItem(a + b);
            }
        });
        PackageObjectTest.<ImArray<Integer>>assertEqualsAB(ImArray.fromObjects(2, 3), arr.value);
    }
    public void testAppend() {
        PackageObjectTest.<ImArray<Integer>>assertEqualsAB(ImArray.fromObjects(1, 0, 2, 3, 1), ImArray.fromObjects(1, 0, 2).chain().appendCollection(ImArray.fromObjects(3, 1)).toArray());
    }
    public void testPreppend() {
        PackageObjectTest.<ImArray<Integer>>assertEqualsAB(ImArray.fromObjects(3, 1, 1, 0, 2), ImArray.fromObjects(1, 0, 2).chain().prependCollection(ImArray.fromObjects(3, 1)).toArray());
    }
    public void testMul() {
        PackageObjectTest.<ImArray<Tuple<Integer, Integer>>>assertEqualsAB(ImArray.fromObjects(new Tuple<Integer, Integer>(1, 3), new Tuple<Integer, Integer>(1, 1), new Tuple<Integer, Integer>(0, 3), new Tuple<Integer, Integer>(0, 1), new Tuple<Integer, Integer>(2, 3), new Tuple<Integer, Integer>(2, 1)), ((ImArray<Tuple<Integer, Integer>>)(((ImArray)(ImArray.fromObjects(1, 0, 2).chain().<Integer>mulBy(ImArray.fromObjects(3, 1)).toArray())))));
    }
    public void testTop() {
        PackageObjectTest.<ImArray<Integer>>assertEqualsAB(ImArray.fromObjects(1, 0), ImArray.fromObjects(1, 0, 2).chain().topNumbers(2).toArray());
        PackageObjectTest.assertTrueValue(ImArray.fromObjects(1, 0, 2).chain().topNumbers(0).toArray().isEmpty());
        PackageObjectTest.<ImArray<Integer>>assertEqualsAB(ImArray.fromObjects(1, 0, 2), ImArray.fromObjects(1, 0, 2).chain().topNumbers(4).toArray());
    }
    public void testSort() {
        PackageObjectTest.<ImArray<Integer>>assertEqualsAB(ImArray.fromObjects(0, 1, 2), ImArray.fromObjects(1, 0, 2).chain().<Integer>sort().toArray());
        PackageObjectTest.<ImArray<Integer>>assertEqualsAB(ImArray.fromObjects(2, 1, 0), ImArray.fromObjects(1, 0, 2).chain().<Integer>sortDesc().toArray());
    }
    public void testReverse() {
        PackageObjectTest.<ImArray<Integer>>assertEqualsAB(ImArray.fromObjects(2, 0, 1), ImArray.fromObjects(1, 0, 2).chain().reverse().toArray());
    }
    public void testGroupBy() {
        PackageObjectTest.<Set<ImArray<Integer>>>assertEqualsAB(ImArray.fromObjects(((ImArray<Integer>)(((ImArray)(ImArray.fromObjects(1, 0))))), ImArray.fromObjects(2)).chain().toSet(), ((Set<ImArray<Integer>>)(((Set)(ImArray.fromObjects(1, 0, 2).chain().<Boolean>groupBy(new F<Integer, Boolean>() {
            @Override
            public Boolean apply(final Integer _) {
                return _ <= 1;
            }
        }).<ImArray<Integer>>mapF(new F<Tuple<Boolean, ImArray<Integer>>, ImArray<Integer>>() {
            @Override
            public ImArray<Integer> apply(final Tuple<Boolean, ImArray<Integer>> _) {
                return _.b;
            }
        }).toSet())))));
        PackageObjectTest.<Set<Integer>>assertEqualsAB(ImArray.fromObjects(-1, 3).chain().toSet(), ImArray.fromObjects(1, -2, 3).chain().<Boolean, Integer>groupByStartFold(new F<Integer, Boolean>() {
            @Override
            public Boolean apply(final Integer _) {
                return _ <= 1;
            }
        }, new F0<Integer>() {
            @Override
            public Integer apply() {
                return 0;
            }
        }, new F2<Integer, Integer, Integer>() {
            @Override
            public Integer apply(final Integer a, final Integer b) {
                return a + b;
            }
        }).<Integer>mapF(new F<Tuple<Boolean, Integer>, Integer>() {
            @Override
            public Integer apply(final Tuple<Boolean, Integer> _) {
                return _.b;
            }
        }).toSet());
    }
    public void testDistinct() {
        PackageObjectTest.<ImArray<Integer>>assertEqualsAB(ImArray.fromObjects(1, 3, 4, 2), ImArray.fromObjects(1, 3, 1, 4, 4, 2).chain().distinct().toArray());
    }
    public void testCombinations() {
        PackageObjectTest.<ImArray<Tuple<Integer, Integer>>>assertEqualsAB(ImArray.fromObjects(new Tuple<Integer, Integer>(2, 0), new Tuple<Integer, Integer>(2, 1), new Tuple<Integer, Integer>(0, 1)), ((ImArray<Tuple<Integer, Integer>>)(((ImArray)(ImArray.fromObjects(2, 0, 1).chain().combinations().toArray())))));
    }
    public void testUncombinations() {
        PackageObjectTest.<ImArray<Integer>>assertEqualsAB(ImArray.fromObjects(2, 0, 1), ImArray.fromObjects(new Tuple<Integer, Integer>(2, 0), new Tuple<Integer, Integer>(2, 1), new Tuple<Integer, Integer>(0, 1)).chain().<Integer>uncombinations().toArray());
    }
    public void testNeighbours() {
        PackageObjectTest.<ImArray<Tuple<Integer, Integer>>>assertEqualsAB(ImArray.fromObjects(new Tuple<Integer, Integer>(2, 0), new Tuple<Integer, Integer>(0, 1)), ((ImArray<Tuple<Integer, Integer>>)(((ImArray)(ImArray.fromObjects(2, 0, 1).chain().neighbours().toArray())))));
        PackageObjectTest.<ImArray<Tuple<Integer, Integer>>>assertEqualsAB(ImArray.fromObjects(new Tuple<Integer, Integer>(2, 0), new Tuple<Integer, Integer>(0, 1), new Tuple<Integer, Integer>(1, 2)), ((ImArray<Tuple<Integer, Integer>>)(((ImArray)(ImArray.fromObjects(2, 0, 1).chain().neighboursRing().toArray())))));
    }
    public void testExclude() {
        PackageObjectTest.<ImArray<Integer>>assertEqualsAB(ImArray.fromObjects(0, 3, 0), ImArray.fromObjects(2, 0, 1, 2, 3, 0).chain().excludeCollection(ImArray.fromObjects(2, 1)).toArray());
    }
    public void testIntersect() {
        PackageObjectTest.<ImArray<Integer>>assertEqualsAB(ImArray.fromObjects(2, 1, 2), ImArray.fromObjects(2, 0, 1, 2, 3, 0).chain().intersectCollection(ImArray.fromObjects(2, 1)).toArray());
    }
    public void testFold() {
        PackageObjectTest.<Integer>assertEqualsAB(3, ImArray.fromObjects(2, 0, 1).chain().<Integer>foldStartBy(0, new F2<Integer, Integer, Integer>() {
            @Override
            public Integer apply(final Integer r, final Integer a) {
                return r + a;
            }
        }));
    }
    public void testCount() {
        PackageObjectTest.<Integer>assertEqualsAB(3, ((int)(ImArray.fromObjects(2, 0, 1).chain().count())));
    }
    public void testHead() {
        final Integer __tmp_0p1n = ImArray.fromObjects(2, 0, 1).chain().head();
        if(__tmp_0p1n == null) {
            throw new NullPointerException();
        }
        PackageObjectTest.<Integer>assertEqualsAB(2, __tmp_0p1n);
    }
    public void testLast() {
        final Integer __tmp_0p1n = ImArray.fromObjects(2, 0, 1).chain().last();
        if(__tmp_0p1n == null) {
            throw new NullPointerException();
        }
        PackageObjectTest.<Integer>assertEqualsAB(1, __tmp_0p1n);
    }
    public void testRandom() {
        final Integer __tmp_0p0rp0n = ImArray.fromObjects(2, 0, 1).chain().randomItem();
        if(__tmp_0p0rp0n == null) {
            throw new NullPointerException();
        }
        PackageObjectTest.assertTrueValue(ImArray.fromObjects(2, 0, 1).containsItem(__tmp_0p0rp0n));
    }
    public void testGap() {
        final Tuple<Integer, Integer> __tmp_0p1n = ImArray.fromObjects(2, 0, 1).chain().<Integer>gap();
        if(__tmp_0p1n == null) {
            throw new NullPointerException();
        }
        PackageObjectTest.<Tuple<Integer, Integer>>assertEqualsAB(new Tuple<Integer, Integer>(0, 2), __tmp_0p1n);
    }
    public void testMin() {
        final Integer __tmp_0p1n = ImArray.fromObjects(2, 0, 1).chain().<Integer>min();
        if(__tmp_0p1n == null) {
            throw new NullPointerException();
        }
        PackageObjectTest.<Integer>assertEqualsAB(0, __tmp_0p1n);
    }
    public void testMax() {
        final Integer __tmp_0p1n = ImArray.fromObjects(2, 3, 0, 1).chain().<Integer>max();
        if(__tmp_0p1n == null) {
            throw new NullPointerException();
        }
        PackageObjectTest.<Integer>assertEqualsAB(3, __tmp_0p1n);
    }
    public void testToString() {
        PackageObjectTest.<String>assertEqualsAB("2, 0, 1", ImArray.fromObjects(2, 0, 1).chain().toStringDelimiter(", "));
        PackageObjectTest.<String>assertEqualsAB("[2, 0, 1]", ImArray.fromObjects(2, 0, 1).chain().toStringStartDelimiterEnd("[", ", ", "]"));
    }
    public ChainTest() {
    }
}