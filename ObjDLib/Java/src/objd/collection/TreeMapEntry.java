package objd.collection;

import objd.lang.*;

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
                final TreeMapEntry<K, V> __tmp_0t_1n = p.left;
                if(__tmp_0t_1n == null) {
                    throw new NullPointerException();
                }
                p = __tmp_0t_1n;
            }
            return ((TreeMapEntry<K, V>)(((TreeMapEntry)(p))));
        } else {
            TreeMapEntry<K, V> p = this.parent;
            TreeMapEntry<K, V> ch = this;
            final TreeMapEntry<K, V> __tmp_0f_2b = p.right;
            while(p != null && __tmp_0f_2b != null && __tmp_0f_2b.equals(ch)) {
                ch = ((TreeMapEntry<K, V>)(((TreeMapEntry)(p))));
                p = p.parent;
            }
            return p;
        }
    }
    public TreeMapEntry<K, V> copyParent(final TreeMapEntry<K, V> parent) {
        final TreeMapEntry<K, V> c = new TreeMapEntry<K, V>(this.key, this.value, parent);
        c.left = ((this.left == null) ? (null) : (this.left.copyParent(c)));
        c.right = ((this.right == null) ? (null) : (this.right.copyParent(c)));
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
    public String toString() {
        return String.format("TreeMapEntry(%s, %s, %s)", this.key, this.value, this.parent);
    }
}