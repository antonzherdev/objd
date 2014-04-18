package core.chain;

public class TreeMapTest extends TestCase {
    public void testMain() {
        ERROR: Unknown local map : MTreeMap#C<§^int§, §^string§> = <to>MTreeMap\MTreeMap#C.class\.<dIt>apply\MTreeMap#C<§^int§, §^string§>\;
        ERROR: Unknown <to>\#C.class\.<dIt>assertEquals(a = 0, b = <l>map\MTreeMap#C<§^int§, §^string§>\.<dIo>count\uint\.cast<int>)\void\;
        ERROR: Unknown <to>\#C.class\.<dIt>assertTrue(value = (<l>map\MTreeMap#C<§^int§, §^string§>\.<rdIo>opt(key = 0)\(§^string§)?\ == none<§^string§>))\void\;
        ERROR: Unknown <l>map\MTreeMap#C<§^int§, §^string§>\.<dIo>set(key = 0, value = "test")\void\;
        ERROR: Unknown <to>\#C.class\.<dIt>assertEquals(a = "test", b = <l>map\MTreeMap#C<§^int§, §^string§>\.<rdIo>apply(key = 0)\§^string§\)\void\;
        ERROR: Unknown local tests : [^int] = [-10, -20, -30, 10, 20, -15, 20, 0, 11, 13, -18];
        ERROR: Unknown <l>tests\[^int]\.<rdIo>for(each = i : §^int§ -> void = <l>map\MTreeMap#C<§^int§, §^string§>\.<dIo>set(key = <l>i\§^int§\, value = ("test" + <l>i\§^int§\))\void\)\void\;
        ERROR: Unknown <to>\#C.class\.<dIt>assertEquals(a = <l>tests\[^int]\.<rdI>chain\Chain#C<§^int§>\.<dIu>distinct\Chain#C<§^int§>\.<dIo>count\uint\, b = <l>map\MTreeMap#C<§^int§, §^string§>\.<dIo>count\uint\)\void\;
        ERROR: Unknown <l>tests\[^int]\.<rdI>chain\Chain#C<§^int§>\.<dIu>distinct\Chain#C<§^int§>\.<dIo>for(each = i : §^int§ -> void = <to>\#C.class\.<dIt>assertEquals(a = ("test" + <l>i\§^int§\), b = <l>map\MTreeMap#C<§^int§, §^string§>\.<rdIo>apply(key = <l>i\§^int§\)\§^string§\)\void\)\void\;
        ERROR: Unknown <to>\#C.class\.<dIt>assertEquals(a = [-30, -20, -18, -15, -10, 0, 10, 11, 13, 20], b = <l>map\MTreeMap#C<§^int§, §^string§>\.<eIo>keys\MTreeMapKeySet#C<§^int§>\.<rdI>chain\Chain#C<§^int§>\.<dIu>toArray\[§^int§]\)\void\;
        ERROR: Unknown <l>tests\[^int]\.<rdI>chain\Chain#C<§^int§>\.<dIu>distinct\Chain#C<§^int§>\.<dIo>for(each = i : §^int§ -> void = {
    <to>\#C.class\.<dIt>assertEquals(a = ("test" + <l>i\§^int§\), b = <l>map\MTreeMap#C<§^int§, §^string§>\.<rdIo>apply(key = <l>i\§^int§\)\§^string§\)\void\
    <l>map\MTreeMap#C<§^int§, §^string§>\.<dIo>removeFor(key = <l>i\§^int§\)\(§^string§)?\
    <to>\#C.class\.<dIt>assertTrue(value = (<l>map\MTreeMap#C<§^int§, §^string§>\.<rdIo>opt(key = <l>i\§^int§\)\(§^string§)?\ == none<§^string§>))\void\
})\void\;
        ERROR: Unknown <to>\#C.class\.<dIt>assertEquals(a = 0, b = <l>map\MTreeMap#C<§^int§, §^string§>\.<dIo>count\uint\.cast<int>)\void\;
    }
    public TreeMapTest() {
    }
    static final ClassType<TreeMapTest> type;
}