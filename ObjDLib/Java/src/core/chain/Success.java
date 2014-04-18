package core.chain;

public class Success<T> extends Try<T> {
    public T get;
    public boolean isSuccess() {
    }
    public boolean isFailure() {
    }
    public Object reason() {
    }
    public Try<R> mapF(F<T, R> f) {
    }
    public Success(T get) {
    }
    static ClassType<Success<T>> type;
}