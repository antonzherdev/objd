package core.chain;

public class MTreeMapKeySet<K> extends TreeMapKeySet_impl<K> {
    public final MTreeMap<K, ?> map;
    @Override
    public int count() {
        return this.map.count();
    }
    @Override
    public Iterator<K> iterator() {
        return TreeMapKeyIterator().applyMapEntry<K>(this.map, this.map.firstEntry());
    }
    public MIterator<K> mutableIterator() {
        return MTreeMapKeyIterator().applyMapEntry<K>(this.map, this.map.firstEntry());
    }
    @Override
    public Iterator<K> iteratorHigherThanKey(K key) {
        return MTreeMapKeyIterator().applyMapEntry<K>(this.map, this.map.higherEntryThanKey(key));
    }
    public MTreeMapKeySet(MTreeMap<K, ?> map) {
        this.map = map;
    }
}