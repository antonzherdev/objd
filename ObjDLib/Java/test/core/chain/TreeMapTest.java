package core.chain;

import java.util.Arrays;
import test.;
import test.Test;
import test.TestCase;
import test..*;

public class TreeMapTest extends TestCase {
    public void testMain() {
        MTreeMap<Integer, String> map = MTreeMap().apply<Integer, String>();
        ().assertEqualsAB<Integer>(ERROR: Unknown 0, ERROR: Unknown <l>map\MTreeMap#C<§^int§, §^string§>\.<dIo>count\uint\.cast<int>);
        ().assertTrueValue(map.optKey(ERROR: Unknown 0) == null);
        map.setKeyValue(ERROR: Unknown 0, "test");
        ().assertEqualsAB<String>("test", map.applyKey(ERROR: Unknown 0));
        ImArray<Integer> tests = Arrays.asList(ERROR: Unknown -10, ERROR: Unknown -20, ERROR: Unknown -30, ERROR: Unknown 10, ERROR: Unknown 20, ERROR: Unknown -15, ERROR: Unknown 20, ERROR: Unknown 0, ERROR: Unknown 11, ERROR: Unknown 13, ERROR: Unknown -18);
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
        ().assertEqualsAB<ImArray<Integer>>(Arrays.asList(ERROR: Unknown -30, ERROR: Unknown -20, ERROR: Unknown -18, ERROR: Unknown -15, ERROR: Unknown -10, ERROR: Unknown 0, ERROR: Unknown 10, ERROR: Unknown 11, ERROR: Unknown 13, ERROR: Unknown 20), map.keys.chain().toArray());
        tests.chain().distinct().forEach(new P<Integer>() {
            @Override
            public void apply(Integer i) {
                ().assertEqualsAB<String>("test" + i, map.applyKey(i));
                map.removeForKey(i);
                ().assertTrueValue(map.optKey(i) == null);
            }
        });
        ().assertEqualsAB<Integer>(ERROR: Unknown 0, ERROR: Unknown <l>map\MTreeMap#C<§^int§, §^string§>\.<dIo>count\uint\.cast<int>);
    }
    public TreeMapTest() {
    }
}