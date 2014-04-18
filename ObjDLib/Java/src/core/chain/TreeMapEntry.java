package core.chain;

public class TreeMapEntry<K, V> {
    public K key;
    public V value;
    public TreeMapEntry<K, V> parent;
    public TreeMapEntry<K, V> left;
    public TreeMapEntry<K, V> right;
    public int color;
    public TreeMapEntry<K, V> next() {
    }
    public TreeMapEntry<K, V> copyParent(TreeMapEntry<K, V> parent) {
    }
    public TreeMapEntry(K key,V value,TreeMapEntry<K, V> parent) {
    }
    static ClassType<TreeMapEntry<K, V>> type;
}