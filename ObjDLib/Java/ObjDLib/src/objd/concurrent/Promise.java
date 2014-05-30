package objd.concurrent;

import objd.lang.*;

public abstract class Promise<T> extends Future<T> {
    public abstract boolean completeValue(final Try<T> value);
    public abstract boolean successValue(final T value);
    public abstract boolean failureReason(final Object reason);
    public static <T> Promise<T> apply() {
        return new DefaultPromise<T>();
    }
    public Promise() {
    }
    public String toString() {
        return "Promise";
    }
}