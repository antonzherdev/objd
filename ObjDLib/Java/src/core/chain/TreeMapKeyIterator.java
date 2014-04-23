package core.chain;

public class TreeMapKeyIterator<K> extends Iterator_impl<K> {
    public final TreeMap<K, ?> map;
    public TreeMapEntry<K, ?> entry;
    public static TreeMapKeyIterator<K> applyMapEntry(TreeMap<K, ?> map,TreeMapEntry<K, ?> entry) {
        TreeMapKeyIterator<K> ret = new TreeMapKeyIterator<K>(map);
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
        K ret = .key;
        if(this.entry == null) {
            throw new RuntimeException("Not null");
        } else {
            this.entry;
        }
        this.entry = .next();
        return ret;
    }
    public TreeMapKeyIterator(TreeMap<K, ?> map) {
        this.map = map;
    }
}