package objd.lang;

public class ThreadUtil {
    public static void sleepPeriod(double period) {
        try {
            long millis = (long) (period * 1000);
            int nanos = (int) ((period * 1000 - millis) * 1000000);
            Thread.sleep(millis, nanos);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
    }
}
