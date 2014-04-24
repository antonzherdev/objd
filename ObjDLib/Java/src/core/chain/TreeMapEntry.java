package core.chain;

public class TreeMapEntry<K, V> {
    public K key;
    public V value;
    public TreeMapEntry<K, V> parent;
    public TreeMapEntry<K, V> left;
    public TreeMapEntry<K, V> right;
    public int color;
    public TreeMapEntry<K, V> next() {
        if(this.right != null) {
            TreeMapEntry<K, V> p = this.right;
            while(p.left != null) {
                p = ERROR: Unknown {
    local __tmp_0_1 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_0_1\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == none<^TreeMapEntry#C<§K#G§, §V#G§>>)) throw "Not null"
else <l>__tmp_0_1\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
};
            }
            return ((TreeMapEntry<K, V>)p);
        } else {
            TreeMapEntry<K, V> p = this.parent;
            TreeMapEntry<K, V> ch = this;
            while(p != null && ERROR: Unknown {
    local __tmp_0_2 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    return ((<l>__tmp_0_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>) && (<l>__tmp_0_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == <lm>ch\TreeMapEntry#C<§K#G§, §V#G§>\))
}) {
                ch = p;
                p = p.parent;
            }
            return p;
        }
    }
    public TreeMapEntry<K, V> copyParent(final TreeMapEntry<K, V> parent) {
        final TreeMapEntry<K, V> c = new TreeMapEntry<K, V>(this.key, this.value, parent);
        c.left = ERROR: Unknown <TreeMapEntry#C<K#G, V#G>>self.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<dI>copy(parent = some(<l>c\TreeMapEntry#C<§K#G§, §V#G§>\)\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)\TreeMapEntry#C<§K#G§, §V#G§>\;
        c.right = ERROR: Unknown <TreeMapEntry#C<K#G, V#G>>self.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<dI>copy(parent = some(<l>c\TreeMapEntry#C<§K#G§, §V#G§>\)\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)\TreeMapEntry#C<§K#G§, §V#G§>\;
        c.color = this.color;
        return c;
    }
    public TreeMapEntry(final K key, final V value, final TreeMapEntry<K, V> parent) {
        this.key = key;
        this.value = value;
        this.parent = parent;
        this.left = null;
        this.right = null;
        this.color = 0;
    }
}