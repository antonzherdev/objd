package objd.collection;

public interface TreeMapKeySet<K> extends ImIterable<K> {
    Iterator<K> iteratorHigherThanKey(final K key);
    String toString();
}