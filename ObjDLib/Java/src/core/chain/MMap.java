package core.chain;

public interface MMap<K, V> extends Map<K, V>, MIterable<Tuple<K, V>> {
    void setKeyValue(final K key, final V value);
    V removeKey(final K key);
    V applyKeyOrUpdateWith(final K key, final F0<V> orUpdateWith);
    V modifyKeyBy(final K key, final F<V, V> by);
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