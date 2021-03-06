package objd.collection;

import objd.lang.*;

public abstract class ImMap_impl<K, V> extends Map_impl<K, V> implements ImMap<K, V> {
    public ImMap_impl() {
    }
    @Override
    public MMap<K, V> mCopy() {
        final MHashMap<K, V> m = new MHashMap<K, V>();
        m.assignImMap(this);
        return m;
    }
    public ImMap<K, V> addItem(final Tuple<K, V> item) {
        final HashMapBuilder<K, V> builder = new HashMapBuilder<K, V>();
        builder.appendAllItems(((Traversable<Tuple<K, V>>)(((Traversable)(this)))));
        builder.appendItem(item);
        return builder.build();
    }
}