package objd.collection;

import objd.lang.*;

public class MMapDefault<K, V> extends MIterable_impl<Tuple<K, V>> {
    public final MMap<K, V> map;
    public final F<K, V> defaultFunc;
    @Override
    public int count() {
        return this.map.count();
    }
    @Override
    public Iterator<Tuple<K, V>> iterator() {
        return this.map.iterator();
    }
    @Override
    public MIterator<Tuple<K, V>> mutableIterator() {
        return this.map.mutableIterator();
    }
    public V applyKey(final K key) {
        return this.map.applyKeyOrUpdateWith(key, new F0<V>() {
            @Override
            public V apply() {
                return MMapDefault.this.defaultFunc.apply(key);
            }
        });
    }
    public Iterable<K> keys() {
        return this.map.keys();
    }
    public Iterable<V> values() {
        return this.map.values();
    }
    public boolean containsKey(final K key) {
        return this.map.containsKey(key);
    }
    public void setKeyValue(final K key, final V value) {
        this.map.setKeyValue(key, value);
    }
    public V modifyKeyBy(final K key, final F<V, V> by) {
        final V value = by.apply(applyKey(key));
        this.map.setKeyValue(key, value);
        return value;
    }
    @Override
    public void appendItem(final Tuple<K, V> item) {
        this.map.appendItem(item);
    }
    public V removeKey(final K key) {
        return this.map.removeKey(key);
    }
    @Override
    public boolean removeItem(final Tuple<K, V> item) {
        return this.map.removeItem(item);
    }
    @Override
    public void clear() {
        this.map.clear();
    }
    @Override
    public ImMapDefault<K, V> im() {
        return new ImMapDefault<K, V>(this.map.im(), this.defaultFunc);
    }
    @Override
    public ImMapDefault<K, V> imCopy() {
        return new ImMapDefault<K, V>(this.map.imCopy(), this.defaultFunc);
    }
    public MMapDefault(final MMap<K, V> map, final F<K, V> defaultFunc) {
        this.map = map;
        this.defaultFunc = defaultFunc;
    }
}