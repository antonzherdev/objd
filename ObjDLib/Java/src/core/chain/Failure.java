package core.chain;

public final class Failure<T> extends Try<T> {
    public final Object reason;
    @Override
    public Object reason() {
        return reason;
    }
    @Override
    public T get() {
        throw new RuntimeException(String.format("Getting failure try: %s", this.reason));
    }
    @Override
    public boolean isSuccess() {
        return false;
    }
    @Override
    public boolean isFailure() {
        return true;
    }
    @Override
    public  <R> Try<R> mapF(F<T, R> f) {
        return ((Try<R>)this);
    }
    public Failure(Object reason) {
        this.reason = reason;
    }
}