package objd.collection;

import objd.lang.*;

public interface TreeMapKeySet<K> extends ImIterable<K> {
    Iterator<K> iteratorHigherThanKey(final K key);
}