package objd.collection;

public class MTreeMapKeySet<K> extends TreeMapKeySet_impl<K> {
    public final MTreeMap<K, ?> map;
    @Override
    public int count() {
        return this.map.count();
    }
    @Override
    public Iterator<K> iterator() {
        return TreeMapKeyIterator.<K>applyMapEntry(this.map, this.map.firstEntry());
    }
    public MIterator<K> mutableIterator() {
        return MTreeMapKeyIterator.<K>applyMapEntry(this.map, this.map.firstEntry());
    }
    @Override
    public Iterator<K> iteratorHigherThanKey(final K key) {
        return MTreeMapKeyIterator.<K>applyMapEntry(this.map, this.map.higherEntryThanKey(key));
    }
    public MTreeMapKeySet(final MTreeMap<K, ?> map) {
        this.map = map;
    }
}