package objd.collection;

public interface Map<K, V> extends Iterable<Tuple<K, V>> {
    V applyKey(final K key);
    Iterable<K> keys();
    Iterable<V> values();
    boolean containsKey(final K key);
    boolean isValueEqualKeyValue(final K key, final V value);
}