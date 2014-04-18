package core.chain;

public class TreeMapKeyIterator<K> implements Iterator<K> {
    public TreeMap<K, Object> map;
    public TreeMapEntry<K, Object> entry;
    public static TreeMapKeyIterator<K> applyMapEntry(TreeMap<K, Object> map,TreeMapEntry<K, Object> entry) {
    }
    public boolean hasNext() {
    }
    public K next() {
    }
    public TreeMapKeyIterator(TreeMap<K, Object> map) {
    }
    static ClassType<TreeMapKeyIterator<K>> type;
}