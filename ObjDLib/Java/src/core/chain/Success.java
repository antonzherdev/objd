package core.chain;

public class Success<T> extends Try<T> {
    public final T get;
    public boolean isSuccess() {
        return ERROR: Unknown True;
    }
    public boolean isFailure() {
        return ERROR: Unknown False;
    }
    public Object reason() {
        ERROR: Unknown throw "Getting reason for success try";
    }
    public Try<R> mapF(F<T, R> f) {
        return new Success<R>(f.apply(get));
    }
    public Success(T get) {
    }
    static final ClassType<Success<T>> type;
}