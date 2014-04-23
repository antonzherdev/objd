package core.chain;

public abstract class TreeMap<K, V> extends ImMap_impl<K, V> {
    public static final int BLACK = ERROR: Unknown 0;
    public static final int RED = ERROR: Unknown 1;
    public final F2<K, K, Integer> comparator;
    public final TreeMapValues<V> values = new TreeMapValues<V>(this);
    @Override
    public V applyKey(K key) {
        return ERROR: Unknown {
    local __tmp : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <TreeMap#C<K#G, V#G>>self.<dI>entryFor(key = <l>key\§K#G§\)\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == none<^TreeMapEntry#C<§K#G§, §V#G§>>)) throw "Not null"
else <l>__tmp\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
}.value;
    }
    @Override
    public V optKey(K key) {
        return ERROR: Unknown <TreeMap#C<K#G, V#G>>self.<dI>entryFor(key = <l>key\§K#G§\)\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUm>value\§V#G§\;
    }
    public abstract TreeMapEntry<K, V> root();
    @Override
    public boolean isEmpty() {
        return this.root() == null;
    }
    public TreeMapEntry<K, V> entryForKey(K key) {
        TreeMapEntry<K, V> p = this.root();
        ERROR: Unknown while((<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>)) {
    local cmp : int = <TreeMap#C<K#G, V#G>>self.<eIU>comparator\(§K#G§, §K#G§) -> int\.<d>apply( = <l>key\§K#G§\,  = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUm>key\§K#G§\)\int\
    if((<l>cmp\int\ < 0)) (<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
else if((<l>cmp\int\ > 0)) (<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
else break
};
        return p;
    }
    @Override
    public abstract TreeMapKeySet<K> keys();
    @Override
    public Iterator<Tuple2<K, V>> iterator() {
        return TreeMapIterator().applyMapEntry<K, V>(this, this.firstEntry());
    }
    public TreeMapIterator<K, V> iteratorHigherThanKey(K key) {
        return TreeMapIterator().applyMapEntry<K, V>(this, higherEntryThanKey(key));
    }
    public TreeMapEntry<K, V> firstEntry() {
        TreeMapEntry<K, V> p = this.root();
        if(p != null) {
            ERROR: Unknown while((<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>)) (<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\);
        }
        return p;
    }
    public K firstKey() {
        return ERROR: Unknown <TreeMap#C<K#G, V#G>>self.<dI>firstEntry\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUm>key\§K#G§\;
    }
    public K lastKey() {
        return ERROR: Unknown <TreeMap#C<K#G, V#G>>self.<dp>lastEntry\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUm>key\§K#G§\;
    }
    public K lowerKeyThanKey(K key) {
        return ERROR: Unknown <TreeMap#C<K#G, V#G>>self.<dp>lowerEntryThan(key = <l>key\§K#G§\)\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUm>key\§K#G§\;
    }
    public K higherKeyThanKey(K key) {
        return ERROR: Unknown <TreeMap#C<K#G, V#G>>self.<dp>higherEntryThan(key = <l>key\§K#G§\)\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUm>key\§K#G§\;
    }
    private TreeMapEntry<K, V> lowerEntryThanKey(K key) {
        TreeMapEntry<K, V> p = this.root();
        ERROR: Unknown while((<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>)) {
    local cmp : int = <TreeMap#C<K#G, V#G>>self.<eIU>comparator\(§K#G§, §K#G§) -> int\.<d>apply( = <l>key\§K#G§\,  = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUm>key\§K#G§\)\int\
    if((<l>cmp\int\ > 0)) {
    if((<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>)) (<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
else return <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\
}
else {
    if((<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>)) {
    (<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
}
else {
    local var parent : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    local var ch : (^TreeMapEntry#C<§K#G§, §V#G§>)¿ = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\
    while(((<lm>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>) && {
    local __tmp_1_1_0_2 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    return ((<l>__tmp_1_1_0_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>) && (<l>__tmp_1_1_0_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == <lm>ch\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\))
})) {
    (<lm>ch\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\)
    (<lm>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
}
    return <lm>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
}
}
};
        return null;
    }
    private TreeMapEntry<K, V> higherEntryThanKey(K key) {
        TreeMapEntry<K, V> p = this.root();
        ERROR: Unknown while((<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>)) {
    local cmp : int = <TreeMap#C<K#G, V#G>>self.<eIU>comparator\(§K#G§, §K#G§) -> int\.<d>apply( = <l>key\§K#G§\,  = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUm>key\§K#G§\)\int\
    if((<l>cmp\int\ < 0)) {
    if((<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>)) (<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
else return <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\
}
else {
    if((<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>)) {
    (<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
}
else {
    local var parent : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    local var ch : (^TreeMapEntry#C<§K#G§, §V#G§>)¿ = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\
    while(((<lm>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>) && {
    local __tmp_1_1_0_2 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    return ((<l>__tmp_1_1_0_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>) && (<l>__tmp_1_1_0_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == <lm>ch\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\))
})) {
    (<lm>ch\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\)
    (<lm>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
}
    return <lm>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
}
}
};
        return null;
    }
    private TreeMapEntry<K, V> lastEntry() {
        TreeMapEntry<K, V> p = this.root();
        if(p != null) {
            ERROR: Unknown while((<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>)) (<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\);
        }
        return p;
    }
    public TreeMap(F2<K, K, Integer> comparator) {
        this.comparator = comparator;
    }
}