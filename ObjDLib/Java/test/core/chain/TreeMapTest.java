package core.chain;

import test.;
import test.Test;
import test.TestCase;
import test..*;

public class TreeMapTest extends TestCase {
    public void testMain() {
        MTreeMap<Integer, String> map = MTreeMap().apply<Integer, String>();
        ().assertEqualsAB<Integer>(0, ERROR: Unknown <l>map\MTreeMap#C<§^int§, §^string§>\.<dIo>count\uint\.cast<int>);
        ().assertTrueValue(map.optKey(0) == null);
        map.setKeyValue(0, "test");
        ().assertEqualsAB<String>("test", map.applyKey(0));
        ImArray<Integer> tests = ImArray.fromObjects(-10, -20, -30, 10, 20, -15, 20, 0, 11, 13, -18);
        tests.forEach(new P<Integer>() {
            @Override
            public void apply(Integer i) {
                map.setKeyValue(i, "test" + i);
            }
        });
        ().assertEqualsAB<Integer>(tests.chain().distinct().count(), map.count());
        tests.chain().distinct().forEach(new P<Integer>() {
            @Override
            public void apply(Integer i) {
                ().assertEqualsAB<String>("test" + i, map.applyKey(i));
            }
        });
        ().assertEqualsAB<ImArray<Integer>>(ImArray.fromObjects(-30, -20, -18, -15, -10, 0, 10, 11, 13, 20), map.keys.chain().toArray());
        tests.chain().distinct().forEach(new P<Integer>() {
            @Override
            public void apply(Integer i) {
                ().assertEqualsAB<String>("test" + i, map.applyKey(i));
                map.removeForKey(i);
                ().assertTrueValue(map.optKey(i) == null);
            }
        });
        ().assertEqualsAB<Integer>(0, ERROR: Unknown <l>map\MTreeMap#C<§^int§, §^string§>\.<dIo>count\uint\.cast<int>);
    }
    public TreeMapTest() {
    }
}