package core.chain;

import objd.collection.ImArray;
import objd.lang.P;
import test.;
import test.Test;
import test.TestCase;
import test..*;

public class TreeMapTest extends TestCase {
    public void testMain() {
        final MTreeMap<Integer, String> map = MTreeMap.<Integer, String>apply();
        .<Integer>assertEqualsAB(0, ((int)map.count()));
        .assertTrueValue(map.applyKey(0) == null);
        map.setKeyValue(0, "test");
        final String __tmp_4 = map.applyKey(0);
        if(__tmp_4 == null) {
            throw new RuntimeException("Not null");
        }
        .<String>assertEqualsAB("test", __tmp_4);
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
                final String __tmp_8 = map.applyKey(i);
                if(__tmp_8 == null) {
                    throw new RuntimeException("Not null");
                }
                .<String>assertEqualsAB("test" + i, __tmp_8);
            }
        });
        .<ImArray<Integer>>assertEqualsAB(ImArray.fromObjects(-30, -20, -18, -15, -10, 0, 10, 11, 13, 20), map.keys.chain().toArray());
        tests.chain().distinct().forEach(new P<Integer>() {
            @Override
            public void apply(final Integer i) {
                final String __tmp_10_0 = map.applyKey(i);
                if(__tmp_10_0 == null) {
                    throw new RuntimeException("Not null");
                }
                .<String>assertEqualsAB("test" + i, __tmp_10_0);
                map.removeKey(i);
                .assertTrueValue(map.applyKey(i) == null);
            }
        });
        .<Integer>assertEqualsAB(0, ((int)map.count()));
    }
    public TreeMapTest() {
    }
}