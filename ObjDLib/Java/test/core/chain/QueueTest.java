package core.chain;

public class QueueTest extends TestCase {
    public void testDeque() {
        ERROR: Unknown local var q : ImQueue#C<§^int§> = <to>ImQueue\ImQueue#C.class\.<dIt>apply\ImQueue#C<§^int§>\;
        ERROR: Unknown <to>\#C.class\.<dIt>assertTrue(value = <lm>q\ImQueue#C<§^int§>\.<dI>isEmpty\bool\)\void\;
        ERROR: Unknown <to>\#C.class\.<dIt>assertEquals(a = 0, b = <lm>q\ImQueue#C<§^int§>\.<dI>count\uint\.cast<int>)\void\;
        ERROR: Unknown (<lm>q\ImQueue#C<§^int§>\ = <lm>q\ImQueue#C<§^int§>\.<dI>enqueue(item = 1)\ImQueue#C<§^int§>\);
        ERROR: Unknown <to>\#C.class\.<dIt>assertTrue(value = !(<lm>q\ImQueue#C<§^int§>\.<dI>isEmpty\bool\))\void\;
        ERROR: Unknown <to>\#C.class\.<dIt>assertEquals(a = 1, b = <lm>q\ImQueue#C<§^int§>\.<dI>count\uint\.cast<int>)\void\;
        ERROR: Unknown (<lm>q\ImQueue#C<§^int§>\ = <lm>q\ImQueue#C<§^int§>\.<dI>enqueue(item = 2)\ImQueue#C<§^int§>\);
        ERROR: Unknown <to>\#C.class\.<dIt>assertEquals(a = 2, b = <lm>q\ImQueue#C<§^int§>\.<dI>count\uint\.cast<int>)\void\;
        ERROR: Unknown (<lm>q\ImQueue#C<§^int§>\ = <lm>q\ImQueue#C<§^int§>\.<dI>enqueue(item = 3)\ImQueue#C<§^int§>\);
        ERROR: Unknown <to>\#C.class\.<dIt>assertEquals(a = 3, b = <lm>q\ImQueue#C<§^int§>\.<dI>count\uint\.cast<int>)\void\;
        ERROR: Unknown local var p : (^(§^int§)?, ^ImQueue#C<§^int§>) = <lm>q\ImQueue#C<§^int§>\.<dI>dequeue\(^(§^int§)?, ^ImQueue#C<§^int§>)\;
        ERROR: Unknown (<lm>q\ImQueue#C<§^int§>\ = <lm>p\(^(§^int§)?, ^ImQueue#C<§^int§>)\.<eIU>b\^ImQueue#C<§^int§>\);
        ERROR: Unknown <to>\#C.class\.<dIt>assertEquals(a = 1, b = <lm>p\(^(§^int§)?, ^ImQueue#C<§^int§>)\.<eIU>a\^(§^int§)?\.get)\void\;
        ERROR: Unknown <to>\#C.class\.<dIt>assertEquals(a = 2, b = <lm>q\ImQueue#C<§^int§>\.<dI>count\uint\.cast<int>)\void\;
        ERROR: Unknown (<lm>p\(^(§^int§)?, ^ImQueue#C<§^int§>)\ = <lm>q\ImQueue#C<§^int§>\.<dI>dequeue\(^(§^int§)?, ^ImQueue#C<§^int§>)\);
        ERROR: Unknown (<lm>q\ImQueue#C<§^int§>\ = <lm>p\(^(§^int§)?, ^ImQueue#C<§^int§>)\.<eIU>b\^ImQueue#C<§^int§>\);
        ERROR: Unknown <to>\#C.class\.<dIt>assertEquals(a = 2, b = <lm>p\(^(§^int§)?, ^ImQueue#C<§^int§>)\.<eIU>a\^(§^int§)?\.get)\void\;
        ERROR: Unknown <to>\#C.class\.<dIt>assertEquals(a = 1, b = <lm>q\ImQueue#C<§^int§>\.<dI>count\uint\.cast<int>)\void\;
        ERROR: Unknown (<lm>p\(^(§^int§)?, ^ImQueue#C<§^int§>)\ = <lm>q\ImQueue#C<§^int§>\.<dI>dequeue\(^(§^int§)?, ^ImQueue#C<§^int§>)\);
        ERROR: Unknown (<lm>q\ImQueue#C<§^int§>\ = <lm>p\(^(§^int§)?, ^ImQueue#C<§^int§>)\.<eIU>b\^ImQueue#C<§^int§>\);
        ERROR: Unknown <to>\#C.class\.<dIt>assertEquals(a = 3, b = <lm>p\(^(§^int§)?, ^ImQueue#C<§^int§>)\.<eIU>a\^(§^int§)?\.get)\void\;
        ERROR: Unknown <to>\#C.class\.<dIt>assertEquals(a = 0, b = <lm>q\ImQueue#C<§^int§>\.<dI>count\uint\.cast<int>)\void\;
    }
    public QueueTest() {
    }
    static final ClassType<QueueTest> type;
}