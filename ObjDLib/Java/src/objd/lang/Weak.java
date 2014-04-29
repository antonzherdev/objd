package objd.lang;

import objd.lang.*;

public class Weak<T> {
    public final T value;
    public boolean isEmpty() {
        return this.value == null;
    }
    public Weak(final T value) {
        this.value = value;
    }
}