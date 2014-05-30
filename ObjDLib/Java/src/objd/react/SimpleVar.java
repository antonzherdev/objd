package objd.react;

import objd.lang.*;

public final class SimpleVar<T> extends Var<T> {
    public SimpleVar(final T initial) {
        super(initial);
    }
    public String toString() {
        return "SimpleVar";
    }
}