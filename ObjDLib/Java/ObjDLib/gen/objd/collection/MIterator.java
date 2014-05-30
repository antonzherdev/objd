package objd.collection;

import objd.lang.*;

public interface MIterator<T> extends Iterator<T> {
    void remove();
    void setValue(final T value);
    String toString();
}