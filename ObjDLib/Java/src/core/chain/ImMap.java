package core.chain;

public interface ImMap<K, V> extends Map<K, V>, ImIterable<Tuple2<K, V>> {
    ImMap<K, V> addItem(Tuple2<K, V> item);
    @Override
    MMap<K, V> mCopy();
}