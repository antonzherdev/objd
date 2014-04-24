package core.chain;

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
        return ERROR: Unknown {
    local __tmp : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <TreeMap#C<K#G, V#G>>self.<dI>entryFor(key = <l>key\§K#G§\)\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == none<^TreeMapEntry#C<§K#G§, §V#G§>>)) throw "Not null"
else <l>__tmp\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
}.value;
    }
    @Override
    public V optKey(final K key) {
        return ERROR: Unknown <TreeMap#C<K#G, V#G>>self.<dI>entryFor(key = <l>key\§K#G§\)\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUm>value\§V#G§\;
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
        return ERROR: Unknown <TreeMap#C<K#G, V#G>>self.<dI>firstEntry\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUm>key\§K#G§\;
    }
    public K lastKey() {
        return ERROR: Unknown <TreeMap#C<K#G, V#G>>self.<dp>lastEntry\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUm>key\§K#G§\;
    }
    public K lowerKeyThanKey(final K key) {
        return ERROR: Unknown <TreeMap#C<K#G, V#G>>self.<dp>lowerEntryThan(key = <l>key\§K#G§\)\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUm>key\§K#G§\;
    }
    public K higherKeyThanKey(final K key) {
        return ERROR: Unknown <TreeMap#C<K#G, V#G>>self.<dp>higherEntryThan(key = <l>key\§K#G§\)\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUm>key\§K#G§\;
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
                    while(parent != null && ERROR: Unknown {
    local __tmp_1_1_0_2 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    return ((<l>__tmp_1_1_0_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>) && (<l>__tmp_1_1_0_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == <lm>ch\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\))
}) {
                        ch = parent;
                        parent = parent.parent;
                    }
                    return parent;
                }
            }
        }
        return null;
    }
    private TreeMapEntry<K, V> higherEntryThanKey(final K key) {
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
                    while(parent != null && ERROR: Unknown {
    local __tmp_1_1_0_2 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    return ((<l>__tmp_1_1_0_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>) && (<l>__tmp_1_1_0_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == <lm>ch\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\))
}) {
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
        this.BLACK = 0;
        this.RED = 1;
        this.values = new TreeMapValues<V>(this);
    }
}