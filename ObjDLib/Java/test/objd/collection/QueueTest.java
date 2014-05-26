package objd.collection;

import objd.lang.*;
import test.PackageObjectTest;
import test.TestCase;

public class QueueTest extends TestCase {
    public void testDeque() {
        ImQueue<Integer> q = ImQueue.<Integer>apply();
        PackageObjectTest.assertTrueValue(q.isEmpty());
        PackageObjectTest.<Integer>assertEqualsAB(0, ((int)(q.count())));
        q = q.enqueueItem(1);
        PackageObjectTest.assertTrueValue(!(q.isEmpty()));
        PackageObjectTest.<Integer>assertEqualsAB(1, ((int)(q.count())));
        q = q.enqueueItem(2);
        PackageObjectTest.<Integer>assertEqualsAB(2, ((int)(q.count())));
        q = q.enqueueItem(3);
        PackageObjectTest.<Integer>assertEqualsAB(3, ((int)(q.count())));
        Tuple<Integer, ImQueue<Integer>> p = q.dequeue();
        q = p.b;
        final Integer __tmp_12p1n = p.a;
        if(__tmp_12p1n == null) {
            throw new NullPointerException();
        }
        PackageObjectTest.<Integer>assertEqualsAB(1, __tmp_12p1n);
        PackageObjectTest.<Integer>assertEqualsAB(2, ((int)(q.count())));
        p = q.dequeue();
        q = p.b;
        final Integer __tmp_16p1n = p.a;
        if(__tmp_16p1n == null) {
            throw new NullPointerException();
        }
        PackageObjectTest.<Integer>assertEqualsAB(2, __tmp_16p1n);
        PackageObjectTest.<Integer>assertEqualsAB(1, ((int)(q.count())));
        p = q.dequeue();
        q = p.b;
        final Integer __tmp_20p1n = p.a;
        if(__tmp_20p1n == null) {
            throw new NullPointerException();
        }
        PackageObjectTest.<Integer>assertEqualsAB(3, __tmp_20p1n);
        PackageObjectTest.<Integer>assertEqualsAB(0, ((int)(q.count())));
    }
    public QueueTest() {
    }
}