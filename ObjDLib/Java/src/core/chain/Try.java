package core.chain;

public abstract class Try<T> {
    public abstract T get();
    public abstract Object reason();
    public abstract boolean isSuccess();
    public boolean isFailure() {
        return !(this.isSuccess());
    }
    public abstract  <R> Try<R> mapF(F<T, R> f);
    public Try() {
    }
}