package core.chain;

public abstract class Promise<T> extends Future<T> {
    public static  <T> Promise<T> apply() {
        return new DefaultPromise<T>();
    }
    public abstract boolean completeValue(Try<T> value);
    public abstract boolean successValue(T value);
    public abstract boolean failureReason(Object reason);
    public Promise() {
    }
}