package objd.concurrent;

import objd.lang.*;
import java.util.concurrent.atomic.AtomicReference;

public class AtomicObject<T> extends AtomicReference<T> {
    public T value() {
        return this.get();
    }
    public AtomicObject(final T value) {
        super(value);
    }
}