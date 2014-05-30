package objd.collection;

import objd.lang.*;

public class IterableF<T> extends ImIterable_impl<T> {
    public final F0<Iterator<T>> iteratorF;
    @Override
    public Iterator<T> iterator() {
        return this.iteratorF.apply();
    }
    public IterableF(final F0<Iterator<T>> iteratorF) {
        this.iteratorF = iteratorF;
    }
    public String toString() {
        return String.format(")");
    }
}