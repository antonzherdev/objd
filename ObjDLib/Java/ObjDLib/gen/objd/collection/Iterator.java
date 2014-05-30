package objd.collection;

import objd.lang.*;

public interface Iterator<T> {
    boolean hasNext();
    T next();
    String toString();
}