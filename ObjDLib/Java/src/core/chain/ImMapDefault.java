package core.chain;

public class ImMapDefault<K, V> extends ImIterable_impl<Tuple2<K, V>> {
    public final ImMap<K, V> map;
    public final F<K, V> defaultFunc;
    @Override
    public int count() {
        return this.map.count();
    }
    @Override
    public Iterator<Tuple2<K, V>> iterator() {
        return this.map.iterator();
    }
    public V applyKey(K key) {
        V __tmp = this.map.optKey(key);
        if(__tmp != null) {
            return __tmp;
        } else {
            return this.defaultFunc.apply(key);
        }
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
    public boolean isEqualMap(Map<K, V> map) {
        return this.map.equals(map);
    }
    public boolean isEqualMapDefault(ImMapDefault<K, V> mapDefault) {
        return this.map.equals(mapDefault.map);
    }
    @Override
    public int hash() {
        return this.map.hash();
    }
    @Override
    public MMapDefault<K, V> mCopy() {
        return new MMapDefault<K, V>(this.map.mCopy(), this.defaultFunc);
    }
    public ImMapDefault(ImMap<K, V> map,F<K, V> defaultFunc) {
        this.map = map;
        this.defaultFunc = defaultFunc;
    }
}