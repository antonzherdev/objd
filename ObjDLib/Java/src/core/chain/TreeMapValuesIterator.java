package core.chain;

public class TreeMapValuesIterator<V> extends Iterator_impl<V> {
    public TreeMapEntry<?, V> entry;
    public static  <V> TreeMapValuesIterator<V> applyMapEntry(TreeMap<?, V> map,TreeMapEntry<?, V> entry) {
        TreeMapValuesIterator<V> ret = new TreeMapValuesIterator<V>(map);
        ret.entry = entry;
        return ret;
    }
    @Override
    public boolean hasNext() {
        return this.entry != null;
    }
    @Override
    public V next() {
        if(this.entry == null) {
            throw new RuntimeException("Not null");
        } else {
            this.entry;
        }
        V ret = .value;
        if(this.entry == null) {
            throw new RuntimeException("Not null");
        } else {
            this.entry;
        }
        this.entry = .next();
        return ret;
    }
    public TreeMapValuesIterator(TreeMap<?, V> map) {
    }
}