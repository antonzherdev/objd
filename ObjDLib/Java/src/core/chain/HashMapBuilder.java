package core.chain;

public class HashMapBuilder<K, V> implements Builder<Tuple2<K, V>, ImHashMap<K, V>> {
    private MHashMap<K, V> map;
    public void appendItem(Tuple2<K, V> item) {
    }
    public ImHashMap<K, V> build() {
    }
    public HashMapBuilder() {
    }
    static ClassType<HashMapBuilder<K, V>> type;
    public void appendAllItems(Traversable<T> items) {
    }
}