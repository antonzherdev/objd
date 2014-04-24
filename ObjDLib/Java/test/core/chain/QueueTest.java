package core.chain;

import test.;
import test.Test;
import test.TestCase;
import test..*;

public class QueueTest extends TestCase {
    public void testDeque() {
        ImQueue<Integer> q = ImQueue.<Integer>apply();
        .assertTrueValue(q.isEmpty());
        .<Integer>assertEqualsAB(0, ((int)q.count()));
        q = q.enqueueItem(1);
        .assertTrueValue(!(q.isEmpty()));
        .<Integer>assertEqualsAB(1, ((int)q.count()));
        q = q.enqueueItem(2);
        .<Integer>assertEqualsAB(2, ((int)q.count()));
        q = q.enqueueItem(3);
        .<Integer>assertEqualsAB(3, ((int)q.count()));
        Tuple<Integer, ImQueue<Integer>> p = q.dequeue();
        q = p.b;
        .<Integer>assertEqualsAB(1, ERROR: Unknown {
    local __tmp_12 : ^(§^int§)? = <lm>p\(^(§^int§)?, ^ImQueue#C<§^int§>)\.<eIU>a\^(§^int§)?\
    if((<l>__tmp_12\^(§^int§)?\ == none<§^int§>)) throw "Not null"
else <l>__tmp_12\^(§^int§)?\
});
        .<Integer>assertEqualsAB(2, ((int)q.count()));
        p = q.dequeue();
        q = p.b;
        .<Integer>assertEqualsAB(2, ERROR: Unknown {
    local __tmp_16 : ^(§^int§)? = <lm>p\(^(§^int§)?, ^ImQueue#C<§^int§>)\.<eIU>a\^(§^int§)?\
    if((<l>__tmp_16\^(§^int§)?\ == none<§^int§>)) throw "Not null"
else <l>__tmp_16\^(§^int§)?\
});
        .<Integer>assertEqualsAB(1, ((int)q.count()));
        p = q.dequeue();
        q = p.b;
        .<Integer>assertEqualsAB(3, ERROR: Unknown {
    local __tmp_20 : ^(§^int§)? = <lm>p\(^(§^int§)?, ^ImQueue#C<§^int§>)\.<eIU>a\^(§^int§)?\
    if((<l>__tmp_20\^(§^int§)?\ == none<§^int§>)) throw "Not null"
else <l>__tmp_20\^(§^int§)?\
});
        .<Integer>assertEqualsAB(0, ((int)q.count()));
    }
    public QueueTest() {
    }
}