package core.chain;

public class HashMapBuilder<K, V> extends Builder_impl<Tuple2<K, V>, ImHashMap<K, V>> {
    private final MHashMap<K, V> map = new MHashMap<K, V>();
    @Override
    public void appendItem(Tuple2<K, V> item) {
        this.map.setKeyValue(item.a, item.b);
    }
    @Override
    public ImHashMap<K, V> build() {
        return this.map.im();
    }
    public HashMapBuilder() {
    }
}