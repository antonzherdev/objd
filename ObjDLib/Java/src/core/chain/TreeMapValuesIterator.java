package core.chain;

public class TreeMapValuesIterator<V> implements Iterator<V> {
    public final TreeMap<?, V> map;
    public TreeMapEntry<?, V> entry;
    public static TreeMapValuesIterator<V> applyMapEntry(TreeMap<?, V> map,TreeMapEntry<?, V> entry) {
        ERROR: Unknown local ret : TreeMapValuesIterator#C<§V#G§> = <to>TreeMapValuesIterator\TreeMapValuesIterator#C.class\.<tcI>apply(map = <l>map\TreeMap#C<^_, V#G>\)\TreeMapValuesIterator#C<§V#G§>\;
        ERROR: Unknown (<l>ret\TreeMapValuesIterator#C<§V#G§>\.<eIm>entry\(^TreeMapEntry#C<^_, §V#G§>)?\ = <l>entry\(^TreeMapEntry#C<^_, V#G>)?\);
        return ret;
    }
    @Override
    public boolean hasNext() {
        return ERROR: Unknown (<TreeMapValuesIterator#C<V#G>>self.<eIm>entry\(^TreeMapEntry#C<^_, §V#G§>)?\ != none<^TreeMapEntry#C<^_, §V#G§>>);
    }
    @Override
    public V next() {
        ERROR: Unknown local ret : V#G = <TreeMapValuesIterator#C<V#G>>self.<eIm>entry\(^TreeMapEntry#C<^_, §V#G§>)?\.get.<eIUm>value\§V#G§\;
        ERROR: Unknown (<TreeMapValuesIterator#C<V#G>>self.<eIm>entry\(^TreeMapEntry#C<^_, §V#G§>)?\ = <TreeMapValuesIterator#C<V#G>>self.<eIm>entry\(^TreeMapEntry#C<^_, §V#G§>)?\.get.<dI>next\(^TreeMapEntry#C<^_, §V#G§>)?\);
        return ret;
    }
    public TreeMapValuesIterator(TreeMap<?, V> map) {
    }
}