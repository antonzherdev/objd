package objd.collection;

import objd.lang.*;

public class TreeMapValuesIterator<V> extends Iterator_impl<V> {
    public TreeMapEntry<?, V> entry;
    public static <V> TreeMapValuesIterator<V> applyMapEntry(final TreeMap<?, V> map, final TreeMapEntry<?, V> entry) {
        final TreeMapValuesIterator<V> ret = new TreeMapValuesIterator<V>(((TreeMap)(map)));
        ret.entry = ((TreeMapEntry)(entry));
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
        this.entry = ((TreeMapEntry)(this.entry.next()));
        return ret;
    }
    public TreeMapValuesIterator(final TreeMap<?, V> map) {
    }
}