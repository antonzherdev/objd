package objd.collection;

import objd.lang.*;

public class TreeMapKeyIterator<K> extends Iterator_impl<K> {
    public final TreeMap<K, ?> map;
    public TreeMapEntry<K, ?> entry;
    public static <K> TreeMapKeyIterator<K> applyMapEntry(final TreeMap<K, ?> map, final TreeMapEntry<K, ?> entry) {
        final TreeMapKeyIterator<K> ret = new TreeMapKeyIterator<K>(map);
        ret.entry = entry;
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
        final TreeMapEntry<K, ?> e = this.entry;
        final K ret = e.key;
        this.entry = e.next();
        return ret;
    }
    public TreeMapKeyIterator(final TreeMap<K, ?> map) {
        this.map = map;
    }
}