package core.chain;

import test.;
import test.Test;
import test.TestCase;
import test..*;

public class QueueTest extends TestCase {
    public void testDeque() {
        ImQueue<Integer> q = ImQueue().apply<Integer>();
        ().assertTrue(q.isEmpty());
        ().assertEquals<Integer>(0, ((int)q.count()));
        q = q.enqueueItem(1);
        ().assertTrue(ERROR: Unknown !(<lm>q\ImQueue#C<§^int§>\.<dI>isEmpty\bool\));
        ().assertEquals<Integer>(1, ((int)q.count()));
        q = q.enqueueItem(2);
        ().assertEquals<Integer>(2, ((int)q.count()));
        q = q.enqueueItem(3);
        ().assertEquals<Integer>(3, ((int)q.count()));
        Tuple2<Integer, ImQueue<Integer>> p = q.dequeue();
        q = p.b;
        ().assertEquals<Integer>(1, ERROR: Unknown {
    local __tmp_12 : ^(§^int§)? = <lm>p\(^(§^int§)?, ^ImQueue#C<§^int§>)\.<eIU>a\^(§^int§)?\
    if((<l>__tmp_12\^(§^int§)?\ == none<§^int§>)) throw "Not null"
else <l>__tmp_12\^(§^int§)?\
});
        ().assertEquals<Integer>(2, ((int)q.count()));
        p = q.dequeue();
        q = p.b;
        ().assertEquals<Integer>(2, ERROR: Unknown {
    local __tmp_16 : ^(§^int§)? = <lm>p\(^(§^int§)?, ^ImQueue#C<§^int§>)\.<eIU>a\^(§^int§)?\
    if((<l>__tmp_16\^(§^int§)?\ == none<§^int§>)) throw "Not null"
else <l>__tmp_16\^(§^int§)?\
});
        ().assertEquals<Integer>(1, ((int)q.count()));
        p = q.dequeue();
        q = p.b;
        ().assertEquals<Integer>(3, ERROR: Unknown {
    local __tmp_20 : ^(§^int§)? = <lm>p\(^(§^int§)?, ^ImQueue#C<§^int§>)\.<eIU>a\^(§^int§)?\
    if((<l>__tmp_20\^(§^int§)?\ == none<§^int§>)) throw "Not null"
else <l>__tmp_20\^(§^int§)?\
});
        ().assertEquals<Integer>(0, ((int)q.count()));
    }
    public QueueTest() {
    }
}