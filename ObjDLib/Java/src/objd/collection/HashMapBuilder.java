package objd.collection;

public class HashMapBuilder<K, V> extends Builder_impl<Tuple<K, V>, ImHashMap<K, V>> {
    private final MHashMap<K, V> map;
    @Override
    public void appendItem(final Tuple<K, V> item) {
        this.map.setKeyValue(item.a, item.b);
    }
    @Override
    public ImHashMap<K, V> build() {
        return this.map.im();
    }
    public HashMapBuilder() {
        this.map = new MHashMap<K, V>();
    }
}