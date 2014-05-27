package objd.react;

import objd.lang.*;
import test.PackageObjectTest;
import org.junit.Test;
import test.TestCase;

public class ObserverTest extends TestCase {
    @Test
    public void testSignal() {
        final Signal<Integer> sig = new Signal<Integer>();
        final Mut<Integer> v = new Mut<Integer>(0);
        final Observer<Integer> o = sig.observeF(new P<Integer>() {
            @Override
            public void apply(final Integer i) {
                v.value = i;
            }
        });
        PackageObjectTest.<Integer>assertEqualsAB(v.value, 0);
        sig.postData(1);
        PackageObjectTest.<Integer>assertEqualsAB(v.value, 1);
        sig.postData(2);
        PackageObjectTest.<Integer>assertEqualsAB(v.value, 2);
        o.detach();
        sig.postData(3);
        PackageObjectTest.<Integer>assertEqualsAB(v.value, 2);
    }
    public ObserverTest() {
    }
    public String toString() {
        return "ObserverTest";
    }
}