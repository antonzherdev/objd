package objd.date;

import java.util.Date;

public class DateTime {
    private final Date date = new Date();

    public double beforeNow() {
        return (double)(System.currentTimeMillis() - date.getTime())/1000.0;
    }

    public double afterNow() {
        return (double)(date.getTime() - System.currentTimeMillis())/1000.0;
    }
}
