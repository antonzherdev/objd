package core.chain;

public class MMapDefault<K, V> extends MIterable_impl<Tuple2<K, V>> {
    public final MMap<K, V> map;
    public final F<K, V> defaultFunc;
    @Override
    public int count() {
        return this.map.count();
    }
    @Override
    public Iterator<Tuple2<K, V>> iterator() {
        return this.map.iterator();
    }
    @Override
    public MIterator<Tuple2<K, V>> mutableIterator() {
        return this.map.mutableIterator();
    }
    public V applyKey(K key) {
        return this.map.objectForKeyOrUpdateWith(key, new F0<V>() {
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
    public boolean containsKey(K key) {
        return this.map.containsKey(key);
    }
    public void setKeyValue(K key,V value) {
        this.map.setKeyValue(key, value);
    }
    public V modifyKeyBy(K key,F<V, V> by) {
        V value = by.apply(applyKey(key));
        this.map.setKeyValue(key, value);
        return value;
    }
    @Override
    public void appendItem(Tuple2<K, V> item) {
        this.map.appendItem(item);
    }
    @Override
    public boolean removeItem(Tuple2<K, V> item) {
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
    public MMapDefault(MMap<K, V> map,F<K, V> defaultFunc) {
        this.map = map;
        this.defaultFunc = defaultFunc;
    }
}