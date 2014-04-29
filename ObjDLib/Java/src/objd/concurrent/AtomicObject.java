package objd.concurrent;

import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicBoolean;

public class AtomicObject<T> extends AtomicReference<T> {
    public T value() {
        return this.get();
    }
    public AtomicObject(final T value) {
        super(value);
    }
}