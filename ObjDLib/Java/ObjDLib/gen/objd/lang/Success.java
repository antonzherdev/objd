package objd.lang;

import objd.lang.*;

public final class Success<T> extends Try<T> {
    public final T get;
    @Override
    public T get() {
        return get;
    }
    @Override
    public T value() {
        return this.get;
    }
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
    public <R> Try<R> mapF(final F<T, R> f) {
        return new Success<R>(f.apply(this.get));
    }
    public Success(final T get) {
        this.get = get;
    }
    public String toString() {
        return String.format("Success(%s)", this.get);
    }
}