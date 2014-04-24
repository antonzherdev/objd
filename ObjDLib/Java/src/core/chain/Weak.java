package core.chain;

public class Weak<T> {
    public final T value;
    public boolean isEmpty() {
        return this.value == null;
    }
    public Weak(final T value) {
        this.value = value;
    }
}