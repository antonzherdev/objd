package objd.react;

import objd.lang.*;

public final class Val<T> extends ImReact<T> {
    public final T value;
    @Override
    public T value() {
        return value;
    }
    public Val(final T value) {
        this.value = value;
    }
    public String toString() {
        return String.format("Val(%s)", this.value);
    }
    public boolean equals(final Object to) {
        if(this == to) {
            return true;
        }
        if(to == null || !(to instanceof Val)) {
            return false;
        }
        final Val<T> o = ((Val<T>)(((Val)(to))));
        return this.value.equals(o.value);
    }
    public int hashCode() {
        int hash = 0;
        hash = hash * 31 + this.value.hashCode();
        return hash;
    }
}