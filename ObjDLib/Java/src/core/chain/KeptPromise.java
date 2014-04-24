package core.chain;

public final class KeptPromise<T> extends Promise<T> {
    public final Try<T> value;
    @Override
    public Try<T> result() {
        return this.value;
    }
    @Override
    public void onCompleteF(final P<Try<T>> f) {
        f.apply(this.value);
    }
    @Override
    public Try<T> waitResultPeriod(final float period) {
        return this.value;
    }
    @Override
    public Try<T> waitResult() {
        return this.value;
    }
    @Override
    public boolean completeValue(final Try<T> value) {
        return false;
    }
    @Override
    public boolean successValue(final T value) {
        return false;
    }
    @Override
    public boolean failureReason(final Object reason) {
        return false;
    }
    public KeptPromise(final Try<T> value) {
        this.value = value;
    }
}