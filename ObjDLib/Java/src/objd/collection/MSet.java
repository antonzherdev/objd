package objd.collection;

import objd.lang.*;

public interface MSet<T> extends Set<T>, MIterable<T> {
    @Override
    ImSet<T> im();
    @Override
    ImSet<T> imCopy();
    String toString();
}