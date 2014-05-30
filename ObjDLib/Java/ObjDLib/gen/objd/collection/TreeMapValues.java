package objd.collection;

public class TreeMapValues<V> extends ImIterable_impl<V> {
    public final TreeMap<Object, V> map;
    @Override
    public int count() {
        return this.map.count();
    }
    @Override
    public Iterator<V> iterator() {
        return TreeMapValuesIterator.<V>applyMapEntry(((TreeMap<Object, V>)(((TreeMap)(this.map)))), ((TreeMapEntry<Object, V>)(((TreeMapEntry)(this.map.firstEntry())))));
    }
    public TreeMapValues(final TreeMap<Object, V> map) {
        this.map = map;
    }
    public String toString() {
        return String.format("TreeMapValues(%s)", this.map);
    }
}