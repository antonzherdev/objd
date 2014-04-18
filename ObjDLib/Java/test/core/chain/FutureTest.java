package core.chain;

public class FutureTest extends TestCase {
    public void testPromiseOnComplete() {
        ERROR: Unknown local n : AtomicInt#C = <to>AtomicInt\AtomicInt#C.class\.<tcI>apply\AtomicInt#C\;
        ERROR: Unknown local count : int = 100000;
        ERROR: Unknown 1.<dIs>to( = <l>count\int\)\Range#C\.<rdIo>for(each = i : §^int§ -> void = {
    local p : Promise#C<§^int§> = <to>Promise\Promise#C.class\.<dIt>apply\Promise#C<§^int§>\
    <to>DispatchQueue\DispatchQueue#C.class\.<eIt>default\DispatchQueue#C\.<dI>async(f =  -> void = return <l>p\Promise#C<§^int§>\.<dIa>success(value = <l>i\§^int§\)\bool\)\void\
    <l>p\Promise#C<§^int§>\.<rdIa>onComplete(f = _ : Try#C<§^int§> -> void = <l>n\AtomicInt#C\.<dI>incrementAndGet\int4\)\void\
})\void\;
        ERROR: Unknown <to>Thread\Thread#C.class\.<dIt>sleep(period = 1.cast<float>)\void\;
        ERROR: Unknown <to>\#C.class\.<dIt>assertEquals(a = <l>n\AtomicInt#C\.<dI>intValue\int4\, b = <l>count\int\.cast<int4>)\void\;
    }
    public void testMap() {
        ERROR: Unknown local n : AtomicInt#C = <to>AtomicInt\AtomicInt#C.class\.<tcI>apply\AtomicInt#C\;
        ERROR: Unknown local count : int = 100;
        ERROR: Unknown local var result : AtomicInt#C = <to>AtomicInt\AtomicInt#C.class\.<tcI>apply\AtomicInt#C\;
        ERROR: Unknown 1.<dIs>to( = <l>count\int\)\Range#C\.<rdIo>parFor(each = i : §^int§ -> void = {
    local p : Promise#C<§^int§> = <to>Promise\Promise#C.class\.<dIt>apply\Promise#C<§^int§>\
    local m : Future#C<§^int§> = <l>p\Promise#C<§^int§>\.<rdI>map(f = _ : §^int§ -> ^int = return (<l>_\§^int§\ + 1))\Future#C<§^int§>\
    <lm>result\AtomicInt#C\.<dI>addAndGet(value = (<l>i\§^int§\ + 1).cast<int4>)\int4\
    <to>DispatchQueue\DispatchQueue#C.class\.<eIt>default\DispatchQueue#C\.<dI>async(f =  -> void = return <l>p\Promise#C<§^int§>\.<dIa>success(value = <l>i\§^int§\)\bool\)\void\
    <l>m\Future#C<§^int§>\.<dIa>onComplete(f = _ : Try#C<§^int§> -> void = <l>n\AtomicInt#C\.<dI>addAndGet(value = <l>_\Try#C<§^int§>\.<dIa>get\§^int§\.cast<int4>)\int4\)\void\
})\void\;
        ERROR: Unknown <to>Thread\Thread#C.class\.<dIt>sleep(period = 3.cast<float>)\void\;
        ERROR: Unknown <to>\#C.class\.<dIt>assertEquals(a = <l>n\AtomicInt#C\, b = <lm>result\AtomicInt#C\)\void\;
    }
    public void testFlatMap() {
        ERROR: Unknown local n : AtomicInt#C = <to>AtomicInt\AtomicInt#C.class\.<tcI>apply\AtomicInt#C\;
        ERROR: Unknown local count : int = 100;
        ERROR: Unknown local var result : int = 0;
        ERROR: Unknown 1.<dIs>to( = <l>count\int\)\Range#C\.<rdIo>for(each = i : §^int§ -> void = {
    local p : Promise#C<§^int§> = <to>Promise\Promise#C.class\.<dIt>apply\Promise#C<§^int§>\
    local m : Future#C<§^int§> = <l>p\Promise#C<§^int§>\.<rdI>flatMap(f = _ : §^int§ -> ^Future#C<§^int§> = return <to>Future\Future#C.class\.<dIt>apply(f =  -> §^int§ = return (<l>_\§^int§\ + 1))\Future#C<§^int§>\)\Future#C<§^int§>\
    (<lm>result\int\ += (<l>i\§^int§\ + 1))
    <to>DispatchQueue\DispatchQueue#C.class\.<eIt>default\DispatchQueue#C\.<dI>async(f =  -> void = return <l>p\Promise#C<§^int§>\.<dIa>success(value = <l>i\§^int§\)\bool\)\void\
    <l>m\Future#C<§^int§>\.<dIa>onComplete(f = _ : Try#C<§^int§> -> void = <l>n\AtomicInt#C\.<dI>addAndGet(value = <l>_\Try#C<§^int§>\.<dIa>get\§^int§\.cast<int4>)\int4\)\void\
})\void\;
        ERROR: Unknown <to>Thread\Thread#C.class\.<dIt>sleep(period = 3.cast<float>)\void\;
        ERROR: Unknown <to>\#C.class\.<dIt>assertEquals(a = <l>n\AtomicInt#C\.<dI>intValue\int4\, b = <lm>result\int\.cast<int4>)\void\;
    }
    public FutureTest() {
    }
    static final ClassType<FutureTest> type;
}