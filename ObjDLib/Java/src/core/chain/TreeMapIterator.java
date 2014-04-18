package core.chain;

public class TreeMapIterator<K, V> implements Iterator<Tuple2<K, V>> {
    public final TreeMap<K, V> map;
    public TreeMapEntry<K, V> entry;
    public static TreeMapIterator<K, V> applyMapEntry(TreeMap<K, V> map,TreeMapEntry<K, V> entry) {
        ERROR: Unknown local ret : TreeMapIterator#C<§K#G§, §V#G§> = <to>TreeMapIterator\TreeMapIterator#C.class\.<tcI>apply(map = <l>map\TreeMap#C<K#G, V#G>\)\TreeMapIterator#C<§K#G§, §V#G§>\;
        ret.entry = entry;
        return ret;
    }
    @Override
    public boolean hasNext() {
        return this.entry != null;
    }
    @Override
    public Tuple2<K, V> next() {
        ERROR: Unknown local ret : (§K#G§, §V#G§) = (<TreeMapIterator#C<K#G, V#G>>self.<eIm>entry\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIUm>key\§K#G§\, <TreeMapIterator#C<K#G, V#G>>self.<eIm>entry\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIUm>value\§V#G§\);
        if(this.entry == null) {
            throw new RuntimeException("Not null");
        } else {
            this.entry;
        }
        this.entry = .next();
        return ret;
    }
    public TreeMapIterator(TreeMap<K, V> map) {
    }
}