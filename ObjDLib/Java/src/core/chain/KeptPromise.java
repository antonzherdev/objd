package core.chain;

public class KeptPromise<T> extends Promise<T> {
    public final Try<T> value;
    public Try<T> result() {
        return ERROR: Unknown some(<KeptPromise#C<T#G>>self.<eIU>value\Try#C<§T#G§>\)\(^Try#C<§T#G§>)?\;
    }
    public void onCompleteF(F<Try<T>, Void> f) {
        f.apply(value);
    }
    public Try<T> waitResultPeriod(float period) {
        return ERROR: Unknown some(<KeptPromise#C<T#G>>self.<eIU>value\Try#C<§T#G§>\)\(^Try#C<§T#G§>)?\;
    }
    public Try<T> waitResult() {
        return value;
    }
    public boolean completeValue(Try<T> value) {
        return ERROR: Unknown False;
    }
    public boolean successValue(T value) {
        return ERROR: Unknown False;
    }
    public boolean failureReason(Object reason) {
        return ERROR: Unknown False;
    }
    public KeptPromise(Try<T> value) {
    }
    static final ClassType<KeptPromise<T>> type;
}