package objd.collection;

import objd.lang.*;

public class MTreeMapKeyIterator<K> extends MIterator_impl<K> {
    public final MTreeMap<K, ?> map;
    private TreeMapEntry<K, ?> prev;
    public TreeMapEntry<K, ?> entry;
    public static <K> MTreeMapKeyIterator<K> applyMapEntry(final MTreeMap<K, ?> map, final TreeMapEntry<K, ?> entry) {
        final MTreeMapKeyIterator<K> ret = new MTreeMapKeyIterator<K>(map);
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
            throw new RuntimeException("Not null");
        }
        final K ret = this.entry.key;
        this.prev = this.entry;
        if(this.entry == null) {
            throw new RuntimeException("Not null");
        }
        this.entry = this.entry.next();
        return ret;
    }
    @Override
    public void remove() {
        {
            final TreeMapEntry<K, ?> _ = this.prev;
            if(_ != null) {
                this.map.deleteEntry(_);
            }
        }
    }
    @Override
    public void setValue(final K value) {
        {
            final TreeMapEntry<K, ?> p = this.prev;
            if(p != null) {
                if(!(p.key.equals(value))) {
                    this.map.deleteEntry(p);
                    this.map.setKeyValue(value, p.value);
                }
            }
        }
    }
    public MTreeMapKeyIterator(final MTreeMap<K, ?> map) {
        this.map = map;
    }
}