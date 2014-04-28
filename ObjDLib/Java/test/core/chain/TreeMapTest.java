package core.chain;

import test.;
import test.Test;
import test.TestCase;
import test..*;

public class TreeMapTest extends TestCase {
    public void testMain() {
        final MTreeMap<Integer, String> map = MTreeMap.<Integer, String>apply();
        .<Integer>assertEqualsAB(0, ((int)map.count()));
        .assertTrueValue(map.optKey(0) == null);
        map.setKeyValue(0, "test");
        .<String>assertEqualsAB("test", map.applyKey(0));
        final ImArray<Integer> tests = ImArray.fromObjects(-10, -20, -30, 10, 20, -15, 20, 0, 11, 13, -18);
        tests.forEach(new P<Integer>() {
            @Override
            public void apply(final Integer i) {
                map.setKeyValue(i, "test" + i);
            }
        });
        .<Integer>assertEqualsAB(tests.chain().distinct().count(), map.count());
        tests.chain().distinct().forEach(new P<Integer>() {
            @Override
            public void apply(final Integer i) {
                .<String>assertEqualsAB("test" + i, map.applyKey(i));
            }
        });
        .<ImArray<Integer>>assertEqualsAB(ImArray.fromObjects(-30, -20, -18, -15, -10, 0, 10, 11, 13, 20), map.keys.chain().toArray());
        tests.chain().distinct().forEach(new P<Integer>() {
            @Override
            public void apply(final Integer i) {
                .<String>assertEqualsAB("test" + i, map.applyKey(i));
                map.removeForKey(i);
                .assertTrueValue(map.optKey(i) == null);
            }
        });
        .<Integer>assertEqualsAB(0, ((int)map.count()));
    }
    public TreeMapTest() {
    }
}