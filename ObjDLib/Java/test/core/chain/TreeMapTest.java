package core.chain;

import test.;
import test.Test;
import test.TestCase;
import test..*;

public class TreeMapTest extends TestCase {
    public void testMain() {
        MTreeMap<Integer, String> map = MTreeMap.<Integer, String>apply();
        .<Integer>assertEqualsAB(0, ((int)map.count()));
        .assertTrueValue(map.optKey(0) == null);
        map.setKeyValue(0, "test");
        .<String>assertEqualsAB("test", map.applyKey(0));
        ImArray<Integer> tests = ImArray.fromObjects(-10, -20, -30, 10, 20, -15, 20, 0, 11, 13, -18);
        {
            Iterator<Integer> __inline__6_i = tests.iterator();
            while(__inline__6_i.hasNext()) {
                Integer i = __inline__6_i.next();
                map.setKeyValue(i, "test" + i);
            }
        }
        .<Integer>assertEqualsAB(tests.chain().distinct().count(), map.count());
        tests.chain().distinct().forEach(new P<Integer>() {
            @Override
            public void apply(Integer i) {
                .<String>assertEqualsAB("test" + i, map.applyKey(i));
            }
        });
        .<ImArray<Integer>>assertEqualsAB(ImArray.fromObjects(-30, -20, -18, -15, -10, 0, 10, 11, 13, 20), map.keys.chain().toArray());
        tests.chain().distinct().forEach(new P<Integer>() {
            @Override
            public void apply(Integer i) {
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