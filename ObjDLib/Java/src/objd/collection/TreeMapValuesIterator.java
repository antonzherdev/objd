package objd.collection;

import objd.lang.*;

public class TreeMapValuesIterator<V> extends Iterator_impl<V> {
    public TreeMapEntry<Object, V> entry;
    public static <V> TreeMapValuesIterator<V> applyMapEntry(final TreeMap<Object, V> map, final TreeMapEntry<Object, V> entry) {
        final TreeMapValuesIterator<V> ret = new TreeMapValuesIterator<V>(((TreeMap<Object, V>)(((TreeMap)(map)))));
        ret.entry = ((TreeMapEntry<Object, V>)(((TreeMapEntry)(entry))));
        return ret;
    }
    @Override
    public boolean hasNext() {
        return this.entry != null;
    }
    @Override
    public V next() {
        if(this.entry == null) {
            throw new NullPointerException();
        }
        final V ret = this.entry.value;
        if(this.entry == null) {
            throw new NullPointerException();
        }
        this.entry = ((TreeMapEntry<Object, V>)(((TreeMapEntry)(this.entry.next()))));
        return ret;
    }
    public TreeMapValuesIterator(final TreeMap<Object, V> map) {
    }
    public String toString() {
        return "TreeMapValuesIterator";
    }
}