package core.chain;

public class KeptPromise<T> extends Promise<T> {
    public Try<T> value;
    public Try<T> result() {
    }
    public void onCompleteF(F<Try<T>, Void> f) {
    }
    public Try<T> waitResultPeriod(float period) {
    }
    public Try<T> waitResult() {
    }
    public boolean completeValue(Try<T> value) {
    }
    public boolean successValue(T value) {
    }
    public boolean failureReason(Object reason) {
    }
    public KeptPromise(Try<T> value) {
    }
    static ClassType<KeptPromise<T>> type;
}