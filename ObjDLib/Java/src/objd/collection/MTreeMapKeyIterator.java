package objd.collection;

import objd.lang.*;

public class MTreeMapKeyIterator<K, V> extends MIterator_impl<K> {
    public final MTreeMap<K, V> map;
    private TreeMapEntry<K, V> prev;
    public TreeMapEntry<K, V> entry;
    public static <K, V> MTreeMapKeyIterator<K, V> applyMapEntry(final MTreeMap<K, V> map, final TreeMapEntry<K, V> entry) {
        final MTreeMapKeyIterator<K, V> ret = new MTreeMapKeyIterator<K, V>(map);
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
        final TreeMapEntry<K, V> e = this.entry;
        final K ret = e.key;
        this.prev = e;
        this.entry = e.next();
        return ret;
    }
    @Override
    public void remove() {
        {
            final TreeMapEntry<K, V> _ = this.prev;
            if(_ != null) {
                this.map.deleteEntry(((TreeMapEntry<K, V>)(((TreeMapEntry)(_)))));
            }
        }
    }
    @Override
    public void setValue(final K value) {
        {
            final TreeMapEntry<K, V> p = this.prev;
            if(p != null) {
                if(!(p.key.equals(value))) {
                    this.map.deleteEntry(((TreeMapEntry<K, V>)(((TreeMapEntry)(p)))));
                    this.map.setKeyValue(value, p.value);
                }
            }
        }
    }
    public MTreeMapKeyIterator(final MTreeMap<K, V> map) {
        this.map = map;
    }
}