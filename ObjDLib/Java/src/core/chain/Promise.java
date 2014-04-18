package core.chain;

public class Promise<T> extends Future<T> {
    public static Promise<T> apply() {
    }
    public abstract boolean completeValue(Try<T> value);
    public abstract boolean successValue(T value);
    public abstract boolean failureReason(Object reason);
    public Promise() {
    }
    static ClassType<Promise<T>> type;
}