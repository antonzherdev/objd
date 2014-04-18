package core.chain;

public class Weak<T> {
    public T value;
    public boolean isEmpty() {
    }
    public Weak(T value) {
    }
    static ClassType<Weak<T>> type;
}