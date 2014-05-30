package objd.collection;

import objd.lang.*;

public class TreeMapKeyIterator<K> extends Iterator_impl<K> {
    public final TreeMap<K, Object> map;
    public TreeMapEntry<K, Object> entry;
    public static <K> TreeMapKeyIterator<K> applyMapEntry(final TreeMap<K, Object> map, final TreeMapEntry<K, Object> entry) {
        final TreeMapKeyIterator<K> ret = new TreeMapKeyIterator<K>(((TreeMap<K, Object>)(((TreeMap)(map)))));
        ret.entry = ((TreeMapEntry<K, Object>)(((TreeMapEntry)(entry))));
        return ret;
    }
    @Override
    public boolean hasNext() {
        return this.entry != null;
    }
    @Override
    public K next() {
        if(this.entry == null) {
            throw new NullPointerException();
        }
        final TreeMapEntry<K, Object> e = this.entry;
        final K ret = e.key;
        this.entry = ((TreeMapEntry<K, Object>)(((TreeMapEntry)(e.next()))));
        return ret;
    }
    public TreeMapKeyIterator(final TreeMap<K, Object> map) {
        this.map = map;
    }
    public String toString() {
        return String.format("TreeMapKeyIterator(%s)", this.map);
    }
}