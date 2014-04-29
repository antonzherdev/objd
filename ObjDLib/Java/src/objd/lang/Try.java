package objd.lang;

import objd.lang.*;

public abstract class Try<T> {
    public abstract T get();
    public abstract Object reason();
    public abstract boolean isSuccess();
    public abstract <R> Try<R> mapF(final F<T, R> f);
    public boolean isFailure() {
        return !(this.isSuccess());
    }
    public Try() {
    }
}