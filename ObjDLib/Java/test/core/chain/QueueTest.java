package core.chain;

public class QueueTest extends TestCase {
    public void testDeque() {
        ERROR: Unknown local var q : ImQueue#C<§^int§> = <to>ImQueue\ImQueue#C.class\.<dIt>apply\ImQueue#C<§^int§>\;
        ().assertTrueValue(q.isEmpty());
        ().assertEqualsAB<Integer>(ERROR: Unknown 0, ERROR: Unknown <lm>q\ImQueue#C<§^int§>\.<dI>count\uint\.cast<int>);
        ERROR: Unknown (<lm>q\ImQueue#C<§^int§>\ = <lm>q\ImQueue#C<§^int§>\.<dI>enqueue(item = 1)\ImQueue#C<§^int§>\);
        ().assertTrueValue(ERROR: Unknown !(<lm>q\ImQueue#C<§^int§>\.<dI>isEmpty\bool\));
        ().assertEqualsAB<Integer>(ERROR: Unknown 1, ERROR: Unknown <lm>q\ImQueue#C<§^int§>\.<dI>count\uint\.cast<int>);
        ERROR: Unknown (<lm>q\ImQueue#C<§^int§>\ = <lm>q\ImQueue#C<§^int§>\.<dI>enqueue(item = 2)\ImQueue#C<§^int§>\);
        ().assertEqualsAB<Integer>(ERROR: Unknown 2, ERROR: Unknown <lm>q\ImQueue#C<§^int§>\.<dI>count\uint\.cast<int>);
        ERROR: Unknown (<lm>q\ImQueue#C<§^int§>\ = <lm>q\ImQueue#C<§^int§>\.<dI>enqueue(item = 3)\ImQueue#C<§^int§>\);
        ().assertEqualsAB<Integer>(ERROR: Unknown 3, ERROR: Unknown <lm>q\ImQueue#C<§^int§>\.<dI>count\uint\.cast<int>);
        ERROR: Unknown local var p : (^(§^int§)?, ^ImQueue#C<§^int§>) = <lm>q\ImQueue#C<§^int§>\.<dI>dequeue\(^(§^int§)?, ^ImQueue#C<§^int§>)\;
        ERROR: Unknown (<lm>q\ImQueue#C<§^int§>\ = <lm>p\(^(§^int§)?, ^ImQueue#C<§^int§>)\.<eIU>b\^ImQueue#C<§^int§>\);
        ().assertEqualsAB<Integer>(ERROR: Unknown 1, ERROR: Unknown <lm>p\(^(§^int§)?, ^ImQueue#C<§^int§>)\.<eIU>a\^(§^int§)?\.get);
        ().assertEqualsAB<Integer>(ERROR: Unknown 2, ERROR: Unknown <lm>q\ImQueue#C<§^int§>\.<dI>count\uint\.cast<int>);
        ERROR: Unknown (<lm>p\(^(§^int§)?, ^ImQueue#C<§^int§>)\ = <lm>q\ImQueue#C<§^int§>\.<dI>dequeue\(^(§^int§)?, ^ImQueue#C<§^int§>)\);
        ERROR: Unknown (<lm>q\ImQueue#C<§^int§>\ = <lm>p\(^(§^int§)?, ^ImQueue#C<§^int§>)\.<eIU>b\^ImQueue#C<§^int§>\);
        ().assertEqualsAB<Integer>(ERROR: Unknown 2, ERROR: Unknown <lm>p\(^(§^int§)?, ^ImQueue#C<§^int§>)\.<eIU>a\^(§^int§)?\.get);
        ().assertEqualsAB<Integer>(ERROR: Unknown 1, ERROR: Unknown <lm>q\ImQueue#C<§^int§>\.<dI>count\uint\.cast<int>);
        ERROR: Unknown (<lm>p\(^(§^int§)?, ^ImQueue#C<§^int§>)\ = <lm>q\ImQueue#C<§^int§>\.<dI>dequeue\(^(§^int§)?, ^ImQueue#C<§^int§>)\);
        ERROR: Unknown (<lm>q\ImQueue#C<§^int§>\ = <lm>p\(^(§^int§)?, ^ImQueue#C<§^int§>)\.<eIU>b\^ImQueue#C<§^int§>\);
        ().assertEqualsAB<Integer>(ERROR: Unknown 3, ERROR: Unknown <lm>p\(^(§^int§)?, ^ImQueue#C<§^int§>)\.<eIU>a\^(§^int§)?\.get);
        ().assertEqualsAB<Integer>(ERROR: Unknown 0, ERROR: Unknown <lm>q\ImQueue#C<§^int§>\.<dI>count\uint\.cast<int>);
    }
    public QueueTest() {
    }
}