package core.chain;

import test.;
import test.Test;
import test.TestCase;
import test..*;

public class ChainTest extends TestCase {
    public void testAnd() {
        ().assertTrueValue(ERROR: Unknown !([True, False, True].<rdI>chain\Chain#C<§^bool§>\.<dIub>and\bool\));
        ().assertTrueValue(ERROR: Unknown !([False, False, False].<rdI>chain\Chain#C<§^bool§>\.<dIub>and\bool\));
        ().assertTrueValue(ERROR: Unknown [True, True, True].chain().and());
        ().assertTrueValue(ERROR: Unknown [].chain().and());
    }
    public void testOr() {
        ().assertTrueValue(ERROR: Unknown [False, False, True].chain().or());
        ().assertTrueValue(ERROR: Unknown !([False, False, False].<rdI>chain\Chain#C<§^bool§>\.<dIub>or\bool\));
        ().assertTrueValue(ERROR: Unknown [True, True, True].chain().or());
        ().assertTrueValue(ERROR: Unknown !([].<rdI>chain\Chain#C<§^_unset§>\.<dIub>or\bool\));
    }
    public void testFuture() {
        repeatTimesF(ERROR: Unknown 1000.cast<uint>, new P0() {
            @Override
            public void apply() {
                ERROR: Unknown local arr : [^(^int, ^Promise#C<^int>)] = 0.<dIsb>to( = 1000)\Range#C\.<rdI>chain\Chain#C<§^int§>\.<dIub>map( = i : §^int§ -> ^(§^int§, ^Promise#C<§^int§>) = return (<l>i\§^int§\, <to>Promise\Promise#C.class\.<dIt>apply\Promise#C<§^int§>\))\Chain#C<§^(§^int§, ^Promise#C<§^int§>)§>\.<dIub>toArray\[§^(§^int§, ^Promise#C<§^int§>)§]\;
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
                ERROR: Unknown local fut : Future#C<^[^int]> = <l>arr\[^(^int, ^Promise#C<^int>)]\.<rdI>chain\Chain#C<§^(^int, ^Promise#C<^int>)§>\.<dIub>map( = _ : §^(^int, ^Promise#C<^int>)§ -> ^Promise#C<^int> = return <l>_\§^(^int, ^Promise#C<^int>)§\.<eIU>b\^Promise#C<^int>\)\Chain#C<§^Promise#C<^int>§>\.<dIub>future(f = chain : Chain#C<§^int§> -> ^[§^int§] = return <l>chain\Chain#C<§^int§>\.<dIub>toArray\[§^int§]\)\Future#C<§^[§^int§]§>\;
                ERROR: Unknown local set : [^int] = <l>arr\[^(^int, ^Promise#C<^int>)]\.<rdI>chain\Chain#C<§^(^int, ^Promise#C<^int>)§>\.<dIub>map( = _ : §^(^int, ^Promise#C<^int>)§ -> ^int = return <l>_\§^(^int, ^Promise#C<^int>)§\.<eIU>a\^int\)\Chain#C<§^int§>\.<dIub>map( = _ : §^int§ -> ^int = return (<l>_\§^int§\ * <l>_\§^int§\))\Chain#C<§^int§>\.<dIub>toArray\[§^int§]\;
                ().assertEqualsAB<ImArray<Integer>>(set, fut.getResultAwait(ERROR: Unknown 5.cast<float>));
            }
        });
    }
    public void testVoidFuture() {
        ERROR: Unknown local arr : [^Promise#C<^void>] = 0.<dIsb>to( = 1000)\Range#C\.<rdI>chain\Chain#C<§^int§>\.<dIub>map( = i : §^int§ -> ^Promise#C<§^void§> = return <to>Promise\Promise#C.class\.<dIt>apply\Promise#C<§^void§>\)\Chain#C<§^Promise#C<§^void§>§>\.<dIub>toArray\[§^Promise#C<§^void§>§]\;
        ERROR: Unknown local fut : Future#C<^void> = <l>arr\[^Promise#C<^void>]\.<rdI>chain\Chain#C<§^Promise#C<^void>§>\.<dIub>voidFuture\Future#C<^void>\;
        ERROR: Unknown local var count : AtomicInt#C = <to>AtomicInt\AtomicInt#C.class\.<tcI>apply\AtomicInt#C\;
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
        ().assertEqualsAB<ImArray<Integer>>(ERROR: Unknown [1, 5, 2, 3, 2], ERROR: Unknown [[1, 5].cast<^int[1]>, [2, 3].cast<^int[1]>, [2]].chain().flat<Integer>().toArray());
    }
    public void testZip() {
        ().assertEqualsAB<ImArray<Integer>>(ERROR: Unknown [2, 3], ERROR: Unknown [1, 0, 3].chain().zipABy<Integer, Integer>(ERROR: Unknown [1, 3], new F2<Integer, Integer, Integer>() {
            @Override
            public Integer apply(Integer a,Integer b) {
                return a + b;
            }
        }).toArray());
    }
    public void testZip3() {
        ().assertEqualsAB<ImArray<Integer>>(ERROR: Unknown [3, 4], ERROR: Unknown [1, 0, 3].chain().zip3ABBy<Integer, Integer, Integer>(ERROR: Unknown [1, 3], ERROR: Unknown [1, 1, 2, 4], new F3<Integer, Integer, Integer, Integer>() {
            @Override
            public Integer apply(Integer a,Integer b,Integer c) {
                return a + b + c;
            }
        }).toArray());
    }
    public void testZipFor() {
        ERROR: Unknown local var arr : [^int] = [];
        ERROR: Unknown [1, 0, 3].chain().zipForABy<Integer>(ERROR: Unknown [1, 3], new P2<Integer, Integer>() {
            @Override
            public void apply(Integer a,Integer b) {
                arr = arr.addItem(a + b);
            }
        });
        ().assertEqualsAB<ImArray<Integer>>(ERROR: Unknown [2, 3], arr);
    }
    public ChainTest() {
    }
}