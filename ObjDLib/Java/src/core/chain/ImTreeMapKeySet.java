package core.chain;

public class ImTreeMapKeySet<K> extends TreeMapKeySet_impl<K> {
    public final TreeMap<K, ?> map;
    @Override
    public int count() {
        return this.map.count();
    }
    @Override
    public Iterator<K> iterator() {
        return TreeMapKeyIterator().applyMapEntry<K>(this.map, this.map.firstEntry());
    }
    @Override
    public Iterator<K> iteratorHigherThanKey(K key) {
        return TreeMapKeyIterator().applyMapEntry<K>(this.map, this.map.higherEntryThanKey(key));
    }
    public ImTreeMapKeySet(TreeMap<K, ?> map) {
        this.map = map;
    }
}