package core.chain;

public class Failure<T> extends Try<T> {
    public Object reason;
    public T get() {
    }
    public boolean isSuccess() {
    }
    public boolean isFailure() {
    }
    public Try<R> mapF(F<T, R> f) {
    }
    public Failure(Object reason) {
    }
    static ClassType<Failure<T>> type;
}