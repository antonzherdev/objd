package objd.concurrent;

import objd.lang.*;
import objd.lang.Try;

public abstract class Promise<T> extends Future<T> {
    public static <T> Promise<T> apply() {
        return new DefaultPromise<T>();
    }
    public abstract boolean completeValue(final Try<T> value);
    public abstract boolean successValue(final T value);
    public abstract boolean failureReason(final Object reason);
    public Promise() {
    }
}