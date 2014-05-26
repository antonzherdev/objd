package objd.collection;

import objd.lang.*;
import test.PackageObjectTest;
import test.TestCase;
import test.Test;
import test.PackageObjectTest.*;

public class TreeMapTest extends TestCase {
    public void testMain() {
        final MTreeMap<Integer, String> map = MTreeMap.<Integer, String>apply();
        PackageObjectTest.<Integer>assertEqualsAB(0, ((int)(map.count())));
        PackageObjectTest.assertTrueValue(map.applyKey(0) == null);
        map.setKeyValue(0, "test");
        final String __tmp_4p1n = map.applyKey(0);
        if(__tmp_4p1n == null) {
            throw new NullPointerException();
        }
        PackageObjectTest.<String>assertEqualsAB("test", __tmp_4p1n);
        final ImArray<Integer> tests = ImArray.fromObjects(-10, -20, -30, 10, 20, -15, 20, 0, 11, 13, -18);
        tests.forEach(new P<Integer>() {
            @Override
            public void apply(final Integer i) {
                map.setKeyValue(i, "test" + i);
            }
        });
        PackageObjectTest.<Integer>assertEqualsAB(tests.chain().distinct().count(), map.count());
        tests.chain().distinct().forEach(new P<Integer>() {
            @Override
            public void apply(final Integer i) {
                final String __tmp_8rp1n = map.applyKey(i);
                if(__tmp_8rp1n == null) {
                    throw new NullPointerException();
                }
                PackageObjectTest.<String>assertEqualsAB("test" + i, __tmp_8rp1n);
            }
        });
        PackageObjectTest.<ImArray<Integer>>assertEqualsAB(ImArray.fromObjects(-30, -20, -18, -15, -10, 0, 10, 11, 13, 20), map.keys.chain().toArray());
        tests.chain().distinct().forEach(new P<Integer>() {
            @Override
            public void apply(final Integer i) {
                final String __tmp_10r_0p1n = map.applyKey(i);
                if(__tmp_10r_0p1n == null) {
                    throw new NullPointerException();
                }
                PackageObjectTest.<String>assertEqualsAB("test" + i, __tmp_10r_0p1n);
                map.removeKey(i);
                PackageObjectTest.assertTrueValue(map.applyKey(i) == null);
            }
        });
        PackageObjectTest.<Integer>assertEqualsAB(0, ((int)(map.count())));
    }
    public TreeMapTest() {
    }
}