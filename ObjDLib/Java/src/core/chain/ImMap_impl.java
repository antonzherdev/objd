package core.chain;

public abstract class ImMap_impl<K, V> extends Map_impl<K, V> implements ImMap<K, V> {
    @Override
    public MMap<K, V> mCopy() {
        MHashMap<K, V> m = new MHashMap<K, V>();
        m.assignImMap(this);
        return m;
    }
    public ImMap<K, V> addItem(Tuple2<K, V> item) {
        HashMapBuilder<K, V> builder = new HashMapBuilder<K, V>();
        builder.appendAllItems(this);
        builder.appendItem(item);
        return builder.build();
    }
}