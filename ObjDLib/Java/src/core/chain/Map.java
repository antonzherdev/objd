package core.chain;

public interface Map<K, V> extends Iterable<Tuple2<K, V>> {
    V applyKey(K key);
    V optKey(K key);
    V getKeyOrValue(K key,V orValue);
    Iterable<K> keys();
    Iterable<V> values();
    boolean containsKey(K key);
    boolean isValueEqualKeyValue(K key,V value);
}