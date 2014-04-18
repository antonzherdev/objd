package core.chain;

public class TreeMapKeyIterator<K> implements Iterator<K> {
    public final TreeMap<K, Object> map;
    public TreeMapEntry<K, Object> entry;
    public static TreeMapKeyIterator<K> applyMapEntry(TreeMap<K, Object> map,TreeMapEntry<K, Object> entry) {
        ERROR: Unknown local ret : TreeMapKeyIterator#C<§K#G§> = <to>TreeMapKeyIterator\TreeMapKeyIterator#C.class\.<tcI>apply(map = <l>map\TreeMap#C<K#G, ^_>\)\TreeMapKeyIterator#C<§K#G§>\;
        ERROR: Unknown (<l>ret\TreeMapKeyIterator#C<§K#G§>\.<eIm>entry\(^TreeMapEntry#C<§K#G§, ^_>)?\ = <l>entry\(^TreeMapEntry#C<K#G, ^_>)?\);
        return ret;
    }
    public boolean hasNext() {
        return ERROR: Unknown (<TreeMapKeyIterator#C<K#G>>self.<eIm>entry\(^TreeMapEntry#C<§K#G§, ^_>)?\ != none<^TreeMapEntry#C<§K#G§, ^_>>);
    }
    public K next() {
        ERROR: Unknown local ret : K#G = <TreeMapKeyIterator#C<K#G>>self.<eIm>entry\(^TreeMapEntry#C<§K#G§, ^_>)?\.get.<eIUm>key\§K#G§\;
        ERROR: Unknown (<TreeMapKeyIterator#C<K#G>>self.<eIm>entry\(^TreeMapEntry#C<§K#G§, ^_>)?\ = <TreeMapKeyIterator#C<K#G>>self.<eIm>entry\(^TreeMapEntry#C<§K#G§, ^_>)?\.get.<dI>next\(^TreeMapEntry#C<§K#G§, ^_>)?\);
        return ret;
    }
    public TreeMapKeyIterator(TreeMap<K, Object> map) {
    }
    static final ClassType<TreeMapKeyIterator<K>> type;
}