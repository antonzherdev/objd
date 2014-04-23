package core.chain;

public final class Success<T> extends Try<T> {
    public final T get;
    @Override
    public boolean isSuccess() {
        return true;
    }
    @Override
    public boolean isFailure() {
        return false;
    }
    @Override
    public Object reason() {
        throw new RuntimeException("Getting reason for success try");
    }
    @Override
    public Try<R> mapF(F<T, R> f) {
        return new Success<R>(f.apply(this.get));
    }
    public Success(T get) {
        this.get = get;
    }
}