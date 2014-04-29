package objd.collection;

import objd.lang.*;

public class TreeMapIterator<K, V> extends Iterator_impl<Tuple<K, V>> {
    public final TreeMap<K, V> map;
    public TreeMapEntry<K, V> entry;
    public static <K, V> TreeMapIterator<K, V> applyMapEntry(final TreeMap<K, V> map, final TreeMapEntry<K, V> entry) {
        final TreeMapIterator<K, V> ret = new TreeMapIterator<K, V>(map);
        ret.entry = entry;
        return ret;
    }
    @Override
    public boolean hasNext() {
        return this.entry != null;
    }
    @Override
    public Tuple<K, V> next() {
        if(this.entry == null) {
            throw new NullPointerException();
        }
        if(this.entry == null) {
            throw new NullPointerException();
        }
        final Tuple<K, V> ret = new Tuple<K, V>(this.entry.key, this.entry.value);
        if(this.entry == null) {
            throw new NullPointerException();
        }
        this.entry = this.entry.next();
        return ret;
    }
    public TreeMapIterator(final TreeMap<K, V> map) {
        this.map = map;
    }
}