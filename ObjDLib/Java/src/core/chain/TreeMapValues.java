package core.chain;

public class TreeMapValues<V> extends ImIterable_impl<V> {
    public final TreeMap<?, V> map;
    @Override
    public int count() {
        return this.map.count();
    }
    @Override
    public Iterator<V> iterator() {
        return TreeMapValuesIterator.<V>applyMapEntry(this.map, this.map.firstEntry());
    }
    public TreeMapValues(TreeMap<?, V> map) {
        this.map = map;
    }
}