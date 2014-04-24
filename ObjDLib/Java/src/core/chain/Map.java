package core.chain;

public interface Map<K, V> extends Iterable<Tuple<K, V>> {
    V applyKey(final K key);
    V optKey(final K key);
    V getKeyOrValue(final K key, final V orValue);
    Iterable<K> keys();
    Iterable<V> values();
    boolean containsKey(final K key);
    boolean isValueEqualKeyValue(final K key, final V value);
}