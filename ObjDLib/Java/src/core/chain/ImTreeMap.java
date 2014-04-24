package core.chain;

public class ImTreeMap<K, V> extends TreeMap<K, V> {
    public final TreeMapEntry<K, V> root;
    @Override
    public TreeMapEntry<K, V> root() {
        return root;
    }
    public final int count;
    @Override
    public int count() {
        return count;
    }
    public final TreeMapKeySet<K> keys = new ImTreeMapKeySet<K>(this);
    @Override
    public TreeMapKeySet<K> keys() {
        return keys;
    }
    @Override
    public boolean isEmpty() {
        return this.root == null;
    }
    @Override
    public MTreeMap<K, V> mCopy() {
        MTreeMap<K, V> m = new MTreeMap<K, V>(this.comparator);
        m.assignImMap(this);
        return m;
    }
    public ImTreeMap(F2<K, K, Integer> comparator,TreeMapEntry<K, V> root,int count) {
        super(comparator);
        this.root = root;
        this.count = count;
    }
}