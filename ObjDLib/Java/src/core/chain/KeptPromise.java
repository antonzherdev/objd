package core.chain;

public class KeptPromise<T> extends Promise<T> {
    public final Try<T> value;
    @Override
    public Try<T> result() {
        return this.value;
    }
    @Override
    public void onCompleteF(P<Try<T>> f) {
        f.apply(this.value);
    }
    @Override
    public Try<T> waitResultPeriod(float period) {
        return this.value;
    }
    @Override
    public Try<T> waitResult() {
        return this.value;
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