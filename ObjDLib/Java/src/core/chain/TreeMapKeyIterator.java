package core.chain;

public class TreeMapKeyIterator<K> implements Iterator<K> {
    public final TreeMap<K, ?> map;
    public TreeMapEntry<K, ?> entry;
    public static TreeMapKeyIterator<K> applyMapEntry(TreeMap<K, ?> map,TreeMapEntry<K, ?> entry) {
        ERROR: Unknown local ret : TreeMapKeyIterator#C<§K#G§> = <to>TreeMapKeyIterator\TreeMapKeyIterator#C.class\.<tcI>apply(map = <l>map\TreeMap#C<K#G, ^_>\)\TreeMapKeyIterator#C<§K#G§>\;
        ret.entry = entry;
        return ret;
    }
    @Override
    public boolean hasNext() {
        return this.entry != null;
    }
    @Override
    public K next() {
        ERROR: Unknown local ret : K#G = <TreeMapKeyIterator#C<K#G>>self.<eIm>entry\(^TreeMapEntry#C<§K#G§, ^_>)?\.get.<eIUm>key\§K#G§\;
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