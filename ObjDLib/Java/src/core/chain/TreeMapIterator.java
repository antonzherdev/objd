package core.chain;

public class TreeMapIterator<K, V> implements Iterator<Tuple2<K, V>> {
    public TreeMap<K, V> map;
    public TreeMapEntry<K, V> entry;
    public static TreeMapIterator<K, V> applyMapEntry(TreeMap<K, V> map,TreeMapEntry<K, V> entry) {
    }
    public boolean hasNext() {
    }
    public Tuple2<K, V> next() {
    }
    public TreeMapIterator(TreeMap<K, V> map) {
    }
    static ClassType<TreeMapIterator<K, V>> type;
}