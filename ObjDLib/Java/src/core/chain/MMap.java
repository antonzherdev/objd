package core.chain;

public interface MMap<K, V> extends Map<K, V>, MIterable<Tuple<K, V>> {
    void setKeyValue(final K key, final V value);
    V removeForKey(final K key);
    V objectForKeyOrUpdateWith(final K key, final F0<V> orUpdateWith);
    V modifyKeyBy(final K key, final F<V, V> by);
    V takeKey(final K key);
    @Override
    void appendItem(final Tuple<K, V> item);
    @Override
    boolean removeItem(final Tuple<K, V> item);
    @Override
    ImMap<K, V> im();
    @Override
    ImMap<K, V> imCopy();
    void assignImMap(final ImMap<K, V> imMap);
}