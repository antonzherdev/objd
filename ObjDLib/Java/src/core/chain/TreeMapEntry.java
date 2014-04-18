package core.chain;

public class TreeMapEntry<K, V> {
    public K key;
    public V value;
    public TreeMapEntry<K, V> parent;
    public TreeMapEntry<K, V> left = ERROR: Unknown none<^TreeMapEntry#C<K#G, V#G>>;
    public TreeMapEntry<K, V> right = ERROR: Unknown none<^TreeMapEntry#C<K#G, V#G>>;
    public int color = ERROR: Unknown 0;
    public TreeMapEntry<K, V> next() {
        ERROR: Unknown if((<TreeMapEntry#C<K#G, V#G>>self.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>)) {
    local var p : (^TreeMapEntry#C<§K#G§, §V#G§>)¿ = <TreeMapEntry#C<K#G, V#G>>self.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\
    while((<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>)) (<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = some(<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get)\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\)
    return <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.cast<^(^TreeMapEntry#C<§K#G§, §V#G§>)?>
}
else {
    local var p : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <TreeMapEntry#C<K#G, V#G>>self.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    local var ch : TreeMapEntry#C<K#G, V#G> = <TreeMapEntry#C<K#G, V#G>>self
    while(((<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>) && {
    local __tmp_0_2 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    return ((<l>__tmp_0_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>) && (<l>__tmp_0_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == <lm>ch\TreeMapEntry#C<§K#G§, §V#G§>\))
})) {
    (<lm>ch\TreeMapEntry#C<§K#G§, §V#G§>\ = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\)
    (<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
}
    return <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
};
    }
    public TreeMapEntry<K, V> copyParent(TreeMapEntry<K, V> parent) {
        ERROR: Unknown local c : TreeMapEntry#C<§K#G§, §V#G§> = <to>TreeMapEntry\TreeMapEntry#C.class\.<tcI>apply(key = <TreeMapEntry#C<K#G, V#G>>self.<eIUm>key\§K#G§\, value = <TreeMapEntry#C<K#G, V#G>>self.<eIUm>value\§V#G§\, parent = <l>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)\TreeMapEntry#C<§K#G§, §V#G§>\;
        ERROR: Unknown (<l>c\TreeMapEntry#C<§K#G§, §V#G§>\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ = <TreeMapEntry#C<K#G, V#G>>self.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<dI>copy(parent = some(<l>c\TreeMapEntry#C<§K#G§, §V#G§>\)\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)\TreeMapEntry#C<§K#G§, §V#G§>\);
        ERROR: Unknown (<l>c\TreeMapEntry#C<§K#G§, §V#G§>\.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ = <TreeMapEntry#C<K#G, V#G>>self.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<dI>copy(parent = some(<l>c\TreeMapEntry#C<§K#G§, §V#G§>\)\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)\TreeMapEntry#C<§K#G§, §V#G§>\);
        ERROR: Unknown (<l>c\TreeMapEntry#C<§K#G§, §V#G§>\.<eIm>color\int\ = <TreeMapEntry#C<K#G, V#G>>self.<eIm>color\int\);
        return c;
    }
    public TreeMapEntry(K key,V value,TreeMapEntry<K, V> parent) {
    }
}