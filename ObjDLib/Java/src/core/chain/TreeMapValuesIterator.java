package core.chain;

public class TreeMapValuesIterator<V> implements Iterator<V> {
    public TreeMapEntry<?, V> entry;
    public static TreeMapValuesIterator<V> applyMapEntry(TreeMap<?, V> map,TreeMapEntry<?, V> entry) {
        ERROR: Unknown local ret : TreeMapValuesIterator#C<§V#G§> = <to>TreeMapValuesIterator\TreeMapValuesIterator#C.class\.<tcI>apply(map = <l>map\TreeMap#C<^_, V#G>\)\TreeMapValuesIterator#C<§V#G§>\;
        ret.entry = entry;
        return ret;
    }
    @Override
    public boolean hasNext() {
        return this.entry != null;
    }
    @Override
    public V next() {
        ERROR: Unknown local ret : V#G = <TreeMapValuesIterator#C<V#G>>self.<eIm>entry\(^TreeMapEntry#C<^_, §V#G§>)?\.get.<eIUm>value\§V#G§\;
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