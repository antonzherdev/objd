package objd.collection;

import objd.lang.*;

public interface ImMap<K, V> extends Map<K, V>, ImIterable<Tuple<K, V>> {
    ImMap<K, V> addItem(final Tuple<K, V> item);
    @Override
    MMap<K, V> mCopy();
    String toString();
}