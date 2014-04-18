package core.chain;

public class ChainTest extends TestCase {
    public void testAnd() {
        ERROR: Unknown <to>\#C.class\.<dIt>assertTrue(value = !([True, False, True].<rdI>chain\Chain#C<§^bool§>\.<dIu>and\bool\))\void\;
        ERROR: Unknown <to>\#C.class\.<dIt>assertTrue(value = !([False, False, False].<rdI>chain\Chain#C<§^bool§>\.<dIu>and\bool\))\void\;
        ERROR: Unknown <to>\#C.class\.<dIt>assertTrue(value = [True, True, True].<rdI>chain\Chain#C<§^bool§>\.<dIu>and\bool\)\void\;
        ERROR: Unknown <to>\#C.class\.<dIt>assertTrue(value = [].<rdI>chain\Chain#C<§^_unset§>\.<dIu>and\bool\)\void\;
    }
    public void testOr() {
        ERROR: Unknown <to>\#C.class\.<dIt>assertTrue(value = [False, False, True].<rdI>chain\Chain#C<§^bool§>\.<dIu>or\bool\)\void\;
        ERROR: Unknown <to>\#C.class\.<dIt>assertTrue(value = !([False, False, False].<rdI>chain\Chain#C<§^bool§>\.<dIu>or\bool\))\void\;
        ERROR: Unknown <to>\#C.class\.<dIt>assertTrue(value = [True, True, True].<rdI>chain\Chain#C<§^bool§>\.<dIu>or\bool\)\void\;
        ERROR: Unknown <to>\#C.class\.<dIt>assertTrue(value = !([].<rdI>chain\Chain#C<§^_unset§>\.<dIu>or\bool\))\void\;
    }
    public void testFuture() {
        ERROR: Unknown <ChainTest#C>self.<rdI>repeat(times = 1000.cast<uint>, f =  -> void = {
    local arr : [^(^int, ^Promise#C<^int>)] = 0.<dIs>to( = 1000)\Range#C\.<rdI>chain\Chain#C<§^int§>\.<dIu>map( = i : §^int§ -> ^(§^int§, ^Promise#C<§^int§>) = return (<l>i\§^int§\, <to>Promise\Promise#C.class\.<dIt>apply\Promise#C<§^int§>\))\Chain#C<§^(§^int§, ^Promise#C<§^int§>)§>\.<dIu>toArray\[§^(§^int§, ^Promise#C<§^int§>)§]\
    <l>arr\[^(^int, ^Promise#C<^int>)]\.<rdIo>for(each = t : §^(^int, ^Promise#C<^int>)§ -> void = <to>DispatchQueue\DispatchQueue#C.class\.<eIt>default\DispatchQueue#C\.<dI>async(f =  -> void = return <l>t\§^(^int, ^Promise#C<^int>)§\.<eIU>b\^Promise#C<^int>\.<dIa>success(value = (<l>t\§^(^int, ^Promise#C<^int>)§\.<eIU>a\^int\ * <l>t\§^(^int, ^Promise#C<^int>)§\.<eIU>a\^int\))\bool\)\void\)\void\
    local fut : Future#C<^[^int]> = <l>arr\[^(^int, ^Promise#C<^int>)]\.<rdI>chain\Chain#C<§^(^int, ^Promise#C<^int>)§>\.<dIu>map( = _ : §^(^int, ^Promise#C<^int>)§ -> ^Promise#C<^int> = return <l>_\§^(^int, ^Promise#C<^int>)§\.<eIU>b\^Promise#C<^int>\)\Chain#C<§^Promise#C<^int>§>\.<dIu>future(f = chain : Chain#C<§^int§> -> ^[§^int§] = return <l>chain\Chain#C<§^int§>\.<dIu>toArray\[§^int§]\)\Future#C<§^[§^int§]§>\
    local set : [^int] = <l>arr\[^(^int, ^Promise#C<^int>)]\.<rdI>chain\Chain#C<§^(^int, ^Promise#C<^int>)§>\.<dIu>map( = _ : §^(^int, ^Promise#C<^int>)§ -> ^int = return <l>_\§^(^int, ^Promise#C<^int>)§\.<eIU>a\^int\)\Chain#C<§^int§>\.<dIu>map( = _ : §^int§ -> ^int = return (<l>_\§^int§\ * <l>_\§^int§\))\Chain#C<§^int§>\.<dIu>toArray\[§^int§]\
    <to>\#C.class\.<dIt>assertEquals(a = <l>set\[^int]\, b = <l>fut\Future#C<^[^int]>\.<dI>getResult(await = 5.cast<float>)\^[^int]\)\void\
})\void\;
    }
    public void testVoidFuture() {
        ERROR: Unknown local arr : [^Promise#C<^void>] = 0.<dIs>to( = 1000)\Range#C\.<rdI>chain\Chain#C<§^int§>\.<dIu>map( = i : §^int§ -> ^Promise#C<§^void§> = return <to>Promise\Promise#C.class\.<dIt>apply\Promise#C<§^void§>\)\Chain#C<§^Promise#C<§^void§>§>\.<dIu>toArray\[§^Promise#C<§^void§>§]\;
        ERROR: Unknown local fut : Future#C<^void> = <l>arr\[^Promise#C<^void>]\.<rdI>chain\Chain#C<§^Promise#C<^void>§>\.<dIu>voidFuture\Future#C<^void>\;
        ERROR: Unknown local var count : AtomicInt#C = <to>AtomicInt\AtomicInt#C.class\.<tcI>apply\AtomicInt#C\;
        ERROR: Unknown <l>arr\[^Promise#C<^void>]\.<rdIo>for(each = p : §^Promise#C<^void>§ -> void = <to>DispatchQueue\DispatchQueue#C.class\.<eIt>default\DispatchQueue#C\.<dI>async(f =  -> void = {
    <lm>count\AtomicInt#C\.<dI>incrementAndGet\int4\
    return <l>p\§^Promise#C<^void>§\.<dIa>success(value = nil)\bool\
})\void\)\void\;
        ERROR: Unknown <to>\#C.class\.<dIt>assertTrue(value = (<l>fut\Future#C<^void>\.<dI>waitResult(period = 5.cast<float>)\(^Try#C<^void>)?\ != none<^Try#C<^void>>))\void\;
        ERROR: Unknown <to>\#C.class\.<dIt>assertEquals(a = <lm>count\AtomicInt#C\.<dI>intValue\int4\, b = <l>arr\[^Promise#C<^void>]\.<rdI>count\uint\.cast<int4>)\void\;
    }
    public void testFlat() {
        ERROR: Unknown <to>\#C.class\.<dIt>assertEquals(a = [1, 5, 2, 3, 2], b = [[1, 5].cast<^int[1]>, [2, 3].cast<^int[1]>, [2]].<rdI>chain\Chain#C<§^^int[1]§>\.<dIu>flat\Chain#C<§^int§>\.<dIu>toArray\[§^int§]\)\void\;
    }
    public void testZip() {
        ERROR: Unknown <to>\#C.class\.<dIt>assertEquals(a = [2, 3], b = [1, 0, 3].<rdI>chain\Chain#C<§^int§>\.<dIu>zip(a = [1, 3], by = a : §^int§, b : §^int§ -> ^int = return (<l>a\§^int§\ + <l>b\§^int§\))\Chain#C<§^int§>\.<dIu>toArray\[§^int§]\)\void\;
    }
    public void testZip3() {
        ERROR: Unknown <to>\#C.class\.<dIt>assertEquals(a = [3, 4], b = [1, 0, 3].<rdI>chain\Chain#C<§^int§>\.<dIu>zip3(a = [1, 3], b = [1, 1, 2, 4], by = a : §^int§, b : §^int§, c : §^int§ -> ^int = return ((<l>a\§^int§\ + <l>b\§^int§\) + <l>c\§^int§\))\Chain#C<§^int§>\.<dIu>toArray\[§^int§]\)\void\;
    }
    public void testZipFor() {
        ERROR: Unknown local var arr : [^int] = [];
        ERROR: Unknown [1, 0, 3].<rdI>chain\Chain#C<§^int§>\.<dIu>zipFor(a = [1, 3], by = a : §^int§, b : §^int§ -> void = (<lm>arr\[^int]\ = <lm>arr\[^int]\.<dIo>add(item = (<l>a\§^int§\ + <l>b\§^int§\))\[^int]\))\void\;
        ERROR: Unknown <to>\#C.class\.<dIt>assertEquals(a = [2, 3], b = <lm>arr\[^int]\)\void\;
    }
    public ChainTest() {
    }
    static final ClassType<ChainTest> type;
}