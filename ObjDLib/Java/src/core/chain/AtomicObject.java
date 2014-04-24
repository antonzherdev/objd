package core.chain;

import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.atomic.AtomicInteger;

public class AtomicObject<T> extends AtomicReference<T> {
    public T value() {
        return this.get();
    }
    public AtomicObject(final T value) {
        super(value);
    }
}