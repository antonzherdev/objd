package objd.collection;

import objd.lang.*;

public class ImTreeMapKeySet<K> extends TreeMapKeySet_impl<K> {
    public final TreeMap<K, ?> map;
    @Override
    public int count() {
        return this.map.count();
    }
    @Override
    public Iterator<K> iterator() {
        return TreeMapKeyIterator.<K>applyMapEntry(((TreeMap)(this.map)), ((TreeMapEntry)(this.map.firstEntry())));
    }
    @Override
    public Iterator<K> iteratorHigherThanKey(final K key) {
        return TreeMapKeyIterator.<K>applyMapEntry(((TreeMap)(this.map)), ((TreeMapEntry)(this.map.higherEntryThanKey(key))));
    }
    public ImTreeMapKeySet(final TreeMap<K, ?> map) {
        this.map = map;
    }
    public String toString() {
        return String.format("ImTreeMapKeySet(%s)", this.map);
    }
}