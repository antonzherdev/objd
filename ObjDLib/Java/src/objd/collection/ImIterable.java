package objd.collection;

import objd.lang.*;

public interface ImIterable<T> extends Iterable<T>, ImTraversable<T> {
    @Override
    MIterable<T> mCopy();
}