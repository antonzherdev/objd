package objd.collection;

import objd.lang.*;

public class MTreeMapIterator<K, V> extends MIterator_impl<Tuple<K, V>> {
    public final MTreeMap<K, V> map;
    private TreeMapEntry<K, V> prev;
    public TreeMapEntry<K, V> entry;
    public static <K, V> MTreeMapIterator<K, V> applyMapEntry(final MTreeMap<K, V> map, final TreeMapEntry<K, V> entry) {
        final MTreeMapIterator<K, V> ret = new MTreeMapIterator<K, V>(map);
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
        final TreeMapEntry<K, V> e = this.entry;
        final Tuple<K, V> ret = new Tuple<K, V>(e.key, e.value);
        this.prev = e;
        this.entry = e.next();
        return ret;
    }
    @Override
    public void remove() {
        {
            final TreeMapEntry<K, V> _ = this.prev;
            if(_ != null) {
                this.map.deleteEntry(_);
            }
        }
    }
    @Override
    public void setValue(final Tuple<K, V> value) {
        {
            final TreeMapEntry<K, V> p = this.prev;
            if(p != null) {
                if(p.key.equals(value.a)) {
                    p.value = value.b;
                } else {
                    this.map.deleteEntry(p);
                    this.map.setKeyValue(value.a, value.b);
                }
            }
        }
    }
    public MTreeMapIterator(final MTreeMap<K, V> map) {
        this.map = map;
    }
}