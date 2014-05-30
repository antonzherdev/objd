package objd.collection;

import objd.lang.*;

public interface ImTraversable<T> extends Traversable<T> {
    MTraversable<T> mCopy();
    String toString();
}