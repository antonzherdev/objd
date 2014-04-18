package core.chain;

public class Failure<T> extends Try<T> {
    public final Object reason;
    public T get() {
        ERROR: Unknown throw "Getting failure try: $<Failure#C<T#G>>self.<eIUo>reason\any\";
    }
    public boolean isSuccess() {
        return ERROR: Unknown False;
    }
    public boolean isFailure() {
        return ERROR: Unknown True;
    }
    public Try<R> mapF(F<T, R> f) {
        return ERROR: Unknown <Failure#C<T#G>>self.cast<Try#C<R#G>>;
    }
    public Failure(Object reason) {
    }
    static final ClassType<Failure<T>> type;
}