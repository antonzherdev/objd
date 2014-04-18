package core.chain;

public class TreeMapValuesIterator<V> implements Iterator<V> {
    public TreeMap<Object, V> map;
    public TreeMapEntry<Object, V> entry;
    public static TreeMapValuesIterator<V> applyMapEntry(TreeMap<Object, V> map,TreeMapEntry<Object, V> entry) {
    }
    public boolean hasNext() {
    }
    public V next() {
    }
    public TreeMapValuesIterator(TreeMap<Object, V> map) {
    }
    static ClassType<TreeMapValuesIterator<V>> type;
}