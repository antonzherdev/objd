package core.chain;

import test.;
import test.Test;
import test.TestCase;
import test..*;

public class QueueTest extends TestCase {
    public void testDeque() {
        ERROR: Unknown local var q : ImQueue#C<§^int§> = <to>ImQueue\ImQueue#C.class\.<dIt>apply\ImQueue#C<§^int§>\;
        ().assertTrueValue(q.isEmpty());
        ().assertEqualsAB<Integer>(ERROR: Unknown 0, ERROR: Unknown <lm>q\ImQueue#C<§^int§>\.<dI>count\uint\.cast<int>);
        q = q.enqueueItem(ERROR: Unknown 1);
        ().assertTrueValue(ERROR: Unknown !(<lm>q\ImQueue#C<§^int§>\.<dI>isEmpty\bool\));
        ().assertEqualsAB<Integer>(ERROR: Unknown 1, ERROR: Unknown <lm>q\ImQueue#C<§^int§>\.<dI>count\uint\.cast<int>);
        q = q.enqueueItem(ERROR: Unknown 2);
        ().assertEqualsAB<Integer>(ERROR: Unknown 2, ERROR: Unknown <lm>q\ImQueue#C<§^int§>\.<dI>count\uint\.cast<int>);
        q = q.enqueueItem(ERROR: Unknown 3);
        ().assertEqualsAB<Integer>(ERROR: Unknown 3, ERROR: Unknown <lm>q\ImQueue#C<§^int§>\.<dI>count\uint\.cast<int>);
        ERROR: Unknown local var p : (^(§^int§)?, ^ImQueue#C<§^int§>) = <lm>q\ImQueue#C<§^int§>\.<dI>dequeue\(^(§^int§)?, ^ImQueue#C<§^int§>)\;
        q = p.b;
        ().assertEqualsAB<Integer>(ERROR: Unknown 1, ERROR: Unknown {
    local __tmp_12 : ^(§^int§)? = <lm>p\(^(§^int§)?, ^ImQueue#C<§^int§>)\.<eIU>a\^(§^int§)?\
    if((<l>__tmp_12\^(§^int§)?\ == none<§^int§>)) throw "Not null"
else <l>__tmp_12\^(§^int§)?\
});
        ().assertEqualsAB<Integer>(ERROR: Unknown 2, ERROR: Unknown <lm>q\ImQueue#C<§^int§>\.<dI>count\uint\.cast<int>);
        p = q.dequeue();
        q = p.b;
        ().assertEqualsAB<Integer>(ERROR: Unknown 2, ERROR: Unknown {
    local __tmp_16 : ^(§^int§)? = <lm>p\(^(§^int§)?, ^ImQueue#C<§^int§>)\.<eIU>a\^(§^int§)?\
    if((<l>__tmp_16\^(§^int§)?\ == none<§^int§>)) throw "Not null"
else <l>__tmp_16\^(§^int§)?\
});
        ().assertEqualsAB<Integer>(ERROR: Unknown 1, ERROR: Unknown <lm>q\ImQueue#C<§^int§>\.<dI>count\uint\.cast<int>);
        p = q.dequeue();
        q = p.b;
        ().assertEqualsAB<Integer>(ERROR: Unknown 3, ERROR: Unknown {
    local __tmp_20 : ^(§^int§)? = <lm>p\(^(§^int§)?, ^ImQueue#C<§^int§>)\.<eIU>a\^(§^int§)?\
    if((<l>__tmp_20\^(§^int§)?\ == none<§^int§>)) throw "Not null"
else <l>__tmp_20\^(§^int§)?\
});
        ().assertEqualsAB<Integer>(ERROR: Unknown 0, ERROR: Unknown <lm>q\ImQueue#C<§^int§>\.<dI>count\uint\.cast<int>);
    }
    public QueueTest() {
    }
}