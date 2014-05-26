package objd.lang;

public class ThreadUtil {
    public static void sleepPeriod(double period) {
        try {
            Thread.sleep((long) (period * 1000000));
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
    }
}
