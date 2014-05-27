package objd.collection;

import objd.lang.*;

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
    public final TreeMapKeySet<K> keys;
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
        final MTreeMap<K, V> m = new MTreeMap<K, V>(this.comparator);
        m.assignImMap(this);
        return m;
    }
    public ImTreeMap(final F2<K, K, Integer> comparator, final TreeMapEntry<K, V> root, final int count) {
        super(comparator);
        this.root = root;
        this.count = count;
        this.keys = new ImTreeMapKeySet<K>(((TreeMap<K, Object>)(((TreeMap)(this)))));
    }
    public String toString() {
        return String.format("ImTreeMap(%s, %lu)", this.root, this.count);
    }
}