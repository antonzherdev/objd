package core.chain;

import java.util.concurrent.atomic.AtomicReference;

public class AtomicObject<T> extends AtomicReference<T> {
    public T value() {
        return this.get();
    }
    public AtomicObject(T value) {
        super(value);
    }
}