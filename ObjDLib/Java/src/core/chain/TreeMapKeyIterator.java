package core.chain;

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
            throw new RuntimeException("Not null");
        } else {
            this.entry;
        }
        final K ret = .key;
        if(this.entry == null) {
            throw new RuntimeException("Not null");
        } else {
            this.entry;
        }
        this.entry = .next();
        return ret;
    }
    public TreeMapKeyIterator(final TreeMap<K, ?> map) {
        this.map = map;
    }
}