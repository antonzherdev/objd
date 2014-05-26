package objd.collection;

import objd.lang.*;

public interface MIterable<T> extends Iterable<T>, MTraversable<T> {
    MIterator<T> mutableIterator();
    @Override
    boolean removeItem(final T item);
    void mutableFilterBy(final F<T, Boolean> by);
    @Override
    ImIterable<T> im();
    @Override
    ImIterable<T> imCopy();
    String toString();
}