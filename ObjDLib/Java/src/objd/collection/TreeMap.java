package objd.collection;

import objd.lang.*;

public abstract class TreeMap<K, V> extends ImMap_impl<K, V> {
    public static final int BLACK;
    public static final int RED;
    public final F2<K, K, Integer> comparator;
    public final TreeMapValues<V> values;
    @Override
    public TreeMapValues<V> values() {
        return values;
    }
    @Override
    public V applyKey(final K key) {
        final TreeMapEntry<K, V> __tmp = entryForKey(key);
        return (__tmp == null) ? (null) : (__tmp.value);
    }
    public abstract TreeMapEntry<K, V> root();
    @Override
    public boolean isEmpty() {
        return this.root() == null;
    }
    public TreeMapEntry<K, V> entryForKey(final K key) {
        TreeMapEntry<K, V> p = this.root();
        while(p != null) {
            final int cmp = this.comparator.apply(key, p.key);
            if(cmp < 0) {
                p = p.left;
            } else {
                if(cmp > 0) {
                    p = p.right;
                } else {
                    break;
                }
            }
        }
        return p;
    }
    @Override
    public abstract TreeMapKeySet<K> keys();
    @Override
    public Iterator<Tuple<K, V>> iterator() {
        return TreeMapIterator.<K, V>applyMapEntry(this, this.firstEntry());
    }
    public TreeMapIterator<K, V> iteratorHigherThanKey(final K key) {
        return TreeMapIterator.<K, V>applyMapEntry(this, higherEntryThanKey(key));
    }
    public TreeMapEntry<K, V> firstEntry() {
        TreeMapEntry<K, V> p = this.root();
        if(p != null) {
            while(p.left != null) {
                p = p.left;
            }
        }
        return p;
    }
    public K firstKey() {
        final TreeMapEntry<K, V> __tmp = this.firstEntry();
        return (__tmp == null) ? (null) : (__tmp.key);
    }
    public K lastKey() {
        final TreeMapEntry<K, V> __tmp = this.lastEntry();
        return (__tmp == null) ? (null) : (__tmp.key);
    }
    public K lowerKeyThanKey(final K key) {
        final TreeMapEntry<K, V> __tmp = lowerEntryThanKey(key);
        return (__tmp == null) ? (null) : (__tmp.key);
    }
    public K higherKeyThanKey(final K key) {
        final TreeMapEntry<K, V> __tmp = higherEntryThanKey(key);
        return (__tmp == null) ? (null) : (__tmp.key);
    }
    private TreeMapEntry<K, V> lowerEntryThanKey(final K key) {
        TreeMapEntry<K, V> p = this.root();
        while(p != null) {
            final int cmp = this.comparator.apply(key, p.key);
            if(cmp > 0) {
                if(p.right != null) {
                    p = p.right;
                } else {
                    return p;
                }
            } else {
                if(p.left != null) {
                    p = p.left;
                } else {
                    TreeMapEntry<K, V> parent = p.parent;
                    TreeMapEntry<K, V> ch = p;
                    final TreeMapEntry<K, V> __tmp_1_1_0_2 = parent.left;
                    while(parent != null && __tmp_1_1_0_2 != null && __tmp_1_1_0_2.equals(ch)) {
                        ch = parent;
                        parent = parent.parent;
                    }
                    return parent;
                }
            }
        }
        return null;
    }
    public TreeMapEntry<K, V> higherEntryThanKey(final K key) {
        TreeMapEntry<K, V> p = this.root();
        while(p != null) {
            final int cmp = this.comparator.apply(key, p.key);
            if(cmp < 0) {
                if(p.left != null) {
                    p = p.left;
                } else {
                    return p;
                }
            } else {
                if(p.right != null) {
                    p = p.right;
                } else {
                    TreeMapEntry<K, V> parent = p.parent;
                    TreeMapEntry<K, V> ch = p;
                    final TreeMapEntry<K, V> __tmp_1_1_0_2 = parent.right;
                    while(parent != null && __tmp_1_1_0_2 != null && __tmp_1_1_0_2.equals(ch)) {
                        ch = parent;
                        parent = parent.parent;
                    }
                    return parent;
                }
            }
        }
        return null;
    }
    private TreeMapEntry<K, V> lastEntry() {
        TreeMapEntry<K, V> p = this.root();
        if(p != null) {
            while(p.right != null) {
                p = p.right;
            }
        }
        return p;
    }
    public TreeMap(final F2<K, K, Integer> comparator) {
        this.comparator = comparator;
        this.values = new TreeMapValues<V>(this);
    }
    static {
        BLACK = 0;
        RED = 1;
    }
}