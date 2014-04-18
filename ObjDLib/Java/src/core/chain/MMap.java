package core.chain;

public interface MMap<K, V> extends Map<K, V>, MIterable<Tuple2<K, V>> {
    void setKeyValue(K key,V value);
    V removeForKey(K key);
    V objectForKeyOrUpdateWith(K key,F<Void, V> orUpdateWith);
    V modifyKeyBy(K key,F<V, V> by);
    V takeKey(K key);
    void appendItem(Tuple2<K, V> item);
    boolean removeItem(Tuple2<K, V> item);
    ImMap<K, V> im();
    ImMap<K, V> imCopy();
    void assignImMap(ImMap<K, V> imMap);
}