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
        return new Success(ERROR: Unknown <l>f\§T#G§ -> R#G\.<d>apply( = <Success#C<T#G>>self.<eIUo>get\§T#G§\)\R#G\);
    }
    public Success(T get) {
    }
    static final ClassType<Success<T>> type;
}