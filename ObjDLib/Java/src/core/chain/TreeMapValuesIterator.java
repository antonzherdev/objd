package core.chain;

public class TreeMapValuesIterator<V> implements Iterator<V> {
    public final TreeMap<Object, V> map;
    public TreeMapEntry<Object, V> entry;
    public static TreeMapValuesIterator<V> applyMapEntry(TreeMap<Object, V> map,TreeMapEntry<Object, V> entry) {
        ERROR: Unknown local ret : TreeMapValuesIterator#C<§V#G§> = <to>TreeMapValuesIterator\TreeMapValuesIterator#C.class\.<tcI>apply(map = <l>map\TreeMap#C<^_, V#G>\)\TreeMapValuesIterator#C<§V#G§>\;
        ERROR: Unknown (<l>ret\TreeMapValuesIterator#C<§V#G§>\.<eIm>entry\(^TreeMapEntry#C<^_, §V#G§>)?\ = <l>entry\(^TreeMapEntry#C<^_, V#G>)?\);
        return ERROR: Unknown <l>ret\TreeMapValuesIterator#C<§V#G§>\;
    }
    public boolean hasNext() {
        return ERROR: Unknown (<TreeMapValuesIterator#C<V#G>>self.<eIm>entry\(^TreeMapEntry#C<^_, §V#G§>)?\ != none<^TreeMapEntry#C<^_, §V#G§>>);
    }
    public V next() {
        ERROR: Unknown local ret : V#G = <TreeMapValuesIterator#C<V#G>>self.<eIm>entry\(^TreeMapEntry#C<^_, §V#G§>)?\.get.<eIUm>value\§V#G§\;
        ERROR: Unknown (<TreeMapValuesIterator#C<V#G>>self.<eIm>entry\(^TreeMapEntry#C<^_, §V#G§>)?\ = <TreeMapValuesIterator#C<V#G>>self.<eIm>entry\(^TreeMapEntry#C<^_, §V#G§>)?\.get.<dI>next\(^TreeMapEntry#C<^_, §V#G§>)?\);
        return ERROR: Unknown <l>ret\§V#G§\;
    }
    public TreeMapValuesIterator(TreeMap<Object, V> map) {
    }
    static final ClassType<TreeMapValuesIterator<V>> type;
}