package objd.collection;

import objd.lang.*;

public interface ImSet<T> extends Set<T>, ImIterable<T> {
    @Override
    MSet<T> mCopy();
    String toString();
}