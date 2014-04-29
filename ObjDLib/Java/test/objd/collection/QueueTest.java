package objd.collection;

import objd.lang.*;
import test.;
import test.Test;
import test.TestCase;
import test..*;

public class QueueTest extends TestCase {
    public void testDeque() {
        ImQueue<Integer> q = ImQueue.<Integer>apply();
        .assertTrueValue(q.isEmpty());
        .<Integer>assertEqualsAB(0, ((int)(q.count())));
        q = q.enqueueItem(1);
        .assertTrueValue(!(q.isEmpty()));
        .<Integer>assertEqualsAB(1, ((int)(q.count())));
        q = q.enqueueItem(2);
        .<Integer>assertEqualsAB(2, ((int)(q.count())));
        q = q.enqueueItem(3);
        .<Integer>assertEqualsAB(3, ((int)(q.count())));
        Tuple<Integer, ImQueue<Integer>> p = q.dequeue();
        q = p.b;
        final Integer __tmp_12p1 = p.a;
        if(__tmp_12p1 == null) {
            throw new RuntimeException("Not null");
        }
        .<Integer>assertEqualsAB(1, __tmp_12p1);
        .<Integer>assertEqualsAB(2, ((int)(q.count())));
        p = q.dequeue();
        q = p.b;
        final Integer __tmp_16p1 = p.a;
        if(__tmp_16p1 == null) {
            throw new RuntimeException("Not null");
        }
        .<Integer>assertEqualsAB(2, __tmp_16p1);
        .<Integer>assertEqualsAB(1, ((int)(q.count())));
        p = q.dequeue();
        q = p.b;
        final Integer __tmp_20p1 = p.a;
        if(__tmp_20p1 == null) {
            throw new RuntimeException("Not null");
        }
        .<Integer>assertEqualsAB(3, __tmp_20p1);
        .<Integer>assertEqualsAB(0, ((int)(q.count())));
    }
    public QueueTest() {
    }
}