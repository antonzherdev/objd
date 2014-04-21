package core.chain;

public class Failure<T> extends Try<T> {
    public final Object reason;
    @Override
    public T get() {
        throw new RuntimeException(ERROR: Unknown "Getting failure try: $<Failure#C<T#G>>self.<eIUo>reason\any\");
    }
    @Override
    public boolean isSuccess() {
        return ERROR: Unknown False;
    }
    @Override
    public boolean isFailure() {
        return ERROR: Unknown True;
    }
    @Override
    public Try<R> mapF(F<T, R> f) {
        return this.ERROR: Unknown cast<Try#C<R#G>>;
    }
    public Failure(Object reason) {
        this.reason = reason;
    }
}