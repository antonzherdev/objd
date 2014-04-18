package core.chain;

public class Try<T> {
    public abstract T get();
    public abstract Object reason();
    public abstract boolean isSuccess();
    public boolean isFailure() {
    }
    public abstract Try<R> mapF(F<T, R> f);
    public Try() {
    }
    static ClassType<Try<T>> type;
}