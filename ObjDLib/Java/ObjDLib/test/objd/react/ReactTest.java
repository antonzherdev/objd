package objd.react;

import objd.lang.*;
import objd.test.PackageObjectTest;
import org.junit.Test;
import objd.collection.ImArray;
import objd.test.TestCase;

public class ReactTest extends TestCase {
    @Test
    public void testMap() {
        final Var<Integer> v = Var.<Integer>applyInitial(2);
        final React<Integer> m = v.<Integer>mapF(new F<Integer, Integer>() {
            @Override
            public Integer apply(final Integer _) {
                return _ * _;
            }
        });
        final React<Integer> m2 = m.<Integer>mapF(new F<Integer, Integer>() {
            @Override
            public Integer apply(final Integer _) {
                return _ * _;
            }
        });
        PackageObjectTest.<Integer>assertEqualsAB(m.value(), 4);
        PackageObjectTest.<Integer>assertEqualsAB(m2.value(), 16);
        v.setValue(4);
        PackageObjectTest.<Integer>assertEqualsAB(m.value(), 16);
        PackageObjectTest.<Integer>assertEqualsAB(m2.value(), 16 * 16);
    }
    @Test
    public void testReactFlag() {
        final Var<Integer> a1 = Var.<Integer>applyInitial(1);
        final Var<Integer> a2 = Var.<Integer>applyInitial(2);
        final ReactFlag f = new ReactFlag(true, ImArray.fromObjects(((Observable<Object>)(((Observable)(a1)))), ((Observable<Object>)(((Observable)(a2))))));
        PackageObjectTest.assertTrueValue(f.value());
        f.clear();
        PackageObjectTest.assertFalseValue(f.value());
        f.set();
        PackageObjectTest.assertTrueValue(f.value());
        f.clear();
        PackageObjectTest.assertFalseValue(f.value());
        a1.setValue(1);
        PackageObjectTest.assertFalseValue(f.value());
        a1.setValue(2);
        PackageObjectTest.assertTrueValue(f.value());
        f.clear();
        PackageObjectTest.assertFalseValue(f.value());
        a2.setValue(1);
        PackageObjectTest.assertTrueValue(f.value());
        f.clear();
        PackageObjectTest.assertFalseValue(f.value());
        a1.setValue(3);
        a2.setValue(3);
        PackageObjectTest.assertTrueValue(f.value());
    }
    public ReactTest() {
    }
    public String toString() {
        return "ReactTest";
    }
}