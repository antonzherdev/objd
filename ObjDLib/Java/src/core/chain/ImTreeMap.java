package core.chain;

public class ImTreeMap<K, V> extends TreeMap<K, V> {
    public TreeMapEntry<K, V> root;
    public int count;
    public TreeMapKeySet<K> keys;
    public boolean isEmpty() {
    }
    public MTreeMap<K, V> mCopy() {
    }
    public ImTreeMap(F2<K, K, Integer> comparator,TreeMapEntry<K, V> root,int count) {
    }
    static ClassType<ImTreeMap<K, V>> type;
}