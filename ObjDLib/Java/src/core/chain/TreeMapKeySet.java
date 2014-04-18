package core.chain;

public interface TreeMapKeySet<K> extends ImIterable<K> {
    Iterator<K> iteratorHigherThanKey(K key);
}