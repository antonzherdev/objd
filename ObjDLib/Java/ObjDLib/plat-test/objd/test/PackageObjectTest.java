package objd.test;

import org.junit.Assert;

public class PackageObjectTest {
    public static void assertTrueValue(boolean b) {
        Assert.assertTrue(b);
    }
    public static void assertFalseValue(boolean b) {
        Assert.assertFalse(b);
    }

    public static void assertEqualsAB(Object a, Object b) {
        Assert.assertEquals(a, b);
    }
}
