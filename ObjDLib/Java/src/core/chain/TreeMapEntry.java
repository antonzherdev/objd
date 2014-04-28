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
                final TreeMapEntry<K, V> __tmp_0_1 = p.left;
                if(__tmp_0_1 == null) {
                    throw new RuntimeException("Not null");
                } else {
                    __tmp_0_1;
                }
                p = ;
            }
            return ((TreeMapEntry<K, V>)p);
        } else {
            TreeMapEntry<K, V> p = this.parent;
            TreeMapEntry<K, V> ch = this;
            final TreeMapEntry<K, V> __tmp_0_2 = p.right;
            while(p != null && __tmp_0_2 != null && __tmp_0_2.equals(ch)) {
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