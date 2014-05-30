package objd.collection;

public class MTreeMapKeySet<K, V> extends TreeMapKeySet_impl<K> {
    public final MTreeMap<K, V> map;
    @Override
    public int count() {
        return this.map.count();
    }
    @Override
    public Iterator<K> iterator() {
        return TreeMapKeyIterator.<K>applyMapEntry(((TreeMap<K, Object>)(((TreeMap)(this.map)))), ((TreeMapEntry<K, Object>)(((TreeMapEntry)(this.map.firstEntry())))));
    }
    public MIterator<K> mutableIterator() {
        return ((MIterator<K>)(((MIterator)(MTreeMapKeyIterator.<K, V>applyMapEntry(this.map, this.map.firstEntry())))));
    }
    @Override
    public Iterator<K> iteratorHigherThanKey(final K key) {
        return ((Iterator<K>)(((Iterator)(MTreeMapKeyIterator.<K, V>applyMapEntry(this.map, this.map.higherEntryThanKey(key))))));
    }
    public MTreeMapKeySet(final MTreeMap<K, V> map) {
        this.map = map;
    }
    public String toString() {
        return String.format("MTreeMapKeySet(%s)", this.map);
    }
}