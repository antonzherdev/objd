package core.chain;

public class HashMapBuilder<K, V> implements Builder<Tuple2<K, V>, ImHashMap<K, V>> {
    private final MHashMap<K, V> map = new MHashMap<K, V>();
    public void appendItem(Tuple2<K, V> item) {
        map.setKeyValue(item.a, item.b);
    }
    public ImHashMap<K, V> build() {
        return map.im();
    }
    public HashMapBuilder() {
    }
    static final ClassType<HashMapBuilder<K, V>> type;
    public void appendAllItems(Traversable<T> items) {
        items.forEach(ERROR: Unknown _ : §T#G§ -> void = <Builder#T<T#G, C#G>>self.<dIa>append(item = <l>_\§T#G§\)\void\);
    }
}