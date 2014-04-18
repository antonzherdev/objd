package core.chain;

public class DefaultPromise<T> extends Promise<T> {
    private AtomicObject<Object> _state;
    public Try<T> result() {
    }
    public boolean completeValue(Try<T> value) {
    }
    public boolean successValue(T value) {
    }
    public boolean failureReason(Object reason) {
    }
    public void onCompleteF(F<Try<T>, Void> f) {
    }
    public DefaultPromise() {
    }
    static ClassType<DefaultPromise<T>> type;
}