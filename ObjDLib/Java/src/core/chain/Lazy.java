package core.chain;

public class Lazy<T> {
    public F<Void, T> f;
    private T _value;
    private boolean _calculated;
    public boolean isCalculated() {
    }
    public T get() {
    }
    public Lazy(F<Void, T> f) {
    }
    static ClassType<Lazy<T>> type;
}