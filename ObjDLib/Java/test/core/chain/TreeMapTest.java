package core.chain;

public class TreeMapTest extends TestCase {
    public void testMain() {
        ERROR: Unknown local map : MTreeMap#C<§^int§, §^string§> = <to>MTreeMap\MTreeMap#C.class\.<dIt>apply\MTreeMap#C<§^int§, §^string§>\;
        ().assertEqualsAB<Integer>(ERROR: Unknown 0, ERROR: Unknown <l>map\MTreeMap#C<§^int§, §^string§>\.<dIo>count\uint\.cast<int>);
        ().assertTrueValue(ERROR: Unknown (<l>map\MTreeMap#C<§^int§, §^string§>\.<rdIo>opt(key = 0)\(§^string§)?\ == none<§^string§>));
        map.setKeyValue(ERROR: Unknown 0, ERROR: Unknown "test");
        ().assertEqualsAB<String>(ERROR: Unknown "test", map.applyKey(ERROR: Unknown 0));
        ERROR: Unknown local tests : [^int] = [-10, -20, -30, 10, 20, -15, 20, 0, 11, 13, -18];
        tests.forEach(new P<Integer>() {
            @Override
            public void f(Integer i) {
                map.setKeyValue(i, ERROR: Unknown ("test" + <l>i\§^int§\));
            }
        });
        ().assertEqualsAB<Integer>(tests.chain().distinct().count(), map.count());
        tests.chain().distinct().forEach(new P<Integer>() {
            @Override
            public void f(Integer i) {
                ().assertEqualsAB<String>(ERROR: Unknown ("test" + <l>i\§^int§\), map.applyKey(i));
            }
        });
        ().assertEqualsAB<ImArray<Integer>>(ERROR: Unknown [-30, -20, -18, -15, -10, 0, 10, 11, 13, 20], map.keys.chain().toArray());
        tests.chain().distinct().forEach(new P<Integer>() {
            @Override
            public void f(Integer i) {
                ().assertEqualsAB<String>(ERROR: Unknown ("test" + <l>i\§^int§\), map.applyKey(i));
                map.removeForKey(i);
                ().assertTrueValue(ERROR: Unknown (<l>map\MTreeMap#C<§^int§, §^string§>\.<rdIo>opt(key = <l>i\§^int§\)\(§^string§)?\ == none<§^string§>));
            }
        });
        ().assertEqualsAB<Integer>(ERROR: Unknown 0, ERROR: Unknown <l>map\MTreeMap#C<§^int§, §^string§>\.<dIo>count\uint\.cast<int>);
    }
    public TreeMapTest() {
    }
}