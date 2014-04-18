package core.chain;

public class KeptPromise<T> extends Promise<T> {
    public final Try<T> value;
    @Override
    public Try<T> result() {
        return ERROR: Unknown some(<KeptPromise#C<T#G>>self.<eIU>value\Try#C<§T#G§>\)\(^Try#C<§T#G§>)?\;
    }
    @Override
    public void onCompleteF(P<Try<T>> f) {
        f.apply(value);
    }
    @Override
    public Try<T> waitResultPeriod(float period) {
        return ERROR: Unknown some(<KeptPromise#C<T#G>>self.<eIU>value\Try#C<§T#G§>\)\(^Try#C<§T#G§>)?\;
    }
    @Override
    public Try<T> waitResult() {
        return value;
    }
    @Override
    public boolean completeValue(Try<T> value) {
        return ERROR: Unknown False;
    }
    @Override
    public boolean successValue(T value) {
        return ERROR: Unknown False;
    }
    @Override
    public boolean failureReason(Object reason) {
        return ERROR: Unknown False;
    }
    public KeptPromise(Try<T> value) {
    }
}