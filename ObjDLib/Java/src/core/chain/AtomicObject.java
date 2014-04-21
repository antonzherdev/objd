package core.chain;

import java.util.concurrent.atomic.AtomicReference;

public class AtomicObject<T> extends AtomicReference<T> {
    public AtomicObject(T value) {
        this.value = value;
    }
}