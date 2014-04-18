package core.chain;

public class Try<T> {
    public abstract T get();
    public abstract Object reason();
    public abstract boolean isSuccess();
    public boolean isFailure() {
        return ERROR: Unknown !(<Try#C<T#G>>self.<dIa>isSuccess\bool\);
    }
    public abstract Try<R> mapF(F<T, R> f);
    public Try() {
    }
    static final ClassType<Try<T>> type;
}