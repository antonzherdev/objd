package core.chain;

public final class KeptPromise<T> extends Promise<T> {
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
        return false;
    }
    @Override
    public boolean successValue(T value) {
        return false;
    }
    @Override
    public boolean failureReason(Object reason) {
        return false;
    }
    public KeptPromise(Try<T> value) {
        this.value = value;
    }
}