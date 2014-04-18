package core.chain;

public class Cache<X, F> {
    public F<X, F> f;
    private X _lastX;
    private F _lastF;
    public F applyX(X x) {
    }
    public Cache(F<X, F> f) {
    }
    static ClassType<Cache<X, F>> type;
}