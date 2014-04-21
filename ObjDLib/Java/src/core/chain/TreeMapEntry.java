package core.chain;

public class TreeMapEntry<K, V> {
    public K key;
    public V value;
    public TreeMapEntry<K, V> parent;
    public TreeMapEntry<K, V> left = null;
    public TreeMapEntry<K, V> right = null;
    public int color = ERROR: Unknown 0;
    public TreeMapEntry<K, V> next() {
        if(this.right != null) {
            ERROR: Unknown local var p : (^TreeMapEntry#C<§K#G§, §V#G§>)¿ = <TreeMapEntry#C<K#G, V#G>>self.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\;
            ERROR: Unknown while((<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>)) (<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = some(<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get)\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\);
            return ERROR: Unknown <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.cast<^(^TreeMapEntry#C<§K#G§, §V#G§>)?>;
        } else {
            ERROR: Unknown local var p : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <TreeMapEntry#C<K#G, V#G>>self.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
            ERROR: Unknown local var ch : TreeMapEntry#C<K#G, V#G> = <TreeMapEntry#C<K#G, V#G>>self;
            ERROR: Unknown while(((<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>) && {
    local __tmp_0_2 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    return ((<l>__tmp_0_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>) && (<l>__tmp_0_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == <lm>ch\TreeMapEntry#C<§K#G§, §V#G§>\))
})) {
    (<lm>ch\TreeMapEntry#C<§K#G§, §V#G§>\ = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\)
    (<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
};
            return p;
        }
    }
    public TreeMapEntry<K, V> copyParent(TreeMapEntry<K, V> parent) {
        ERROR: Unknown local c : TreeMapEntry#C<§K#G§, §V#G§> = <to>TreeMapEntry\TreeMapEntry#C.class\.<tcI>apply(key = <TreeMapEntry#C<K#G, V#G>>self.<eIUm>key\§K#G§\, value = <TreeMapEntry#C<K#G, V#G>>self.<eIUm>value\§V#G§\, parent = <l>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)\TreeMapEntry#C<§K#G§, §V#G§>\;
        c.left = ERROR: Unknown <TreeMapEntry#C<K#G, V#G>>self.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<dI>copy(parent = some(<l>c\TreeMapEntry#C<§K#G§, §V#G§>\)\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)\TreeMapEntry#C<§K#G§, §V#G§>\;
        c.right = ERROR: Unknown <TreeMapEntry#C<K#G, V#G>>self.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<dI>copy(parent = some(<l>c\TreeMapEntry#C<§K#G§, §V#G§>\)\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)\TreeMapEntry#C<§K#G§, §V#G§>\;
        c.color = this.color;
        return c;
    }
    public TreeMapEntry(K key,V value,TreeMapEntry<K, V> parent) {
        this.key = key;
        this.value = value;
        this.parent = parent;
    }
}