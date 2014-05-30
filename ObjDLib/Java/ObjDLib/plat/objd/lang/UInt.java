package objd.lang;

public class UInt {
    public static int rndMax(int count) {
        return (int) (Math.random()*(count + 1));
    }

    public static int maxB(int a, int b) {
        return a > b ? a : b;
    }
}
