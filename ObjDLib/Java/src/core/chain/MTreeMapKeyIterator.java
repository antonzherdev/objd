package core.chain;

public class MTreeMapKeyIterator<K> implements MIterator<K> {
    public final MTreeMap<K, Object> map;
    private TreeMapEntry<K, Object> prev;
    public TreeMapEntry<K, Object> entry;
    public static MTreeMapKeyIterator<K> applyMapEntry(MTreeMap<K, Object> map,TreeMapEntry<K, Object> entry) {
        ERROR: Unknown local ret : MTreeMapKeyIterator#C<§K#G§> = <to>MTreeMapKeyIterator\MTreeMapKeyIterator#C.class\.<tcI>apply(map = <l>map\MTreeMap#C<K#G, ^_>\)\MTreeMapKeyIterator#C<§K#G§>\;
        ERROR: Unknown (<l>ret\MTreeMapKeyIterator#C<§K#G§>\.<eIm>entry\(^TreeMapEntry#C<§K#G§, ^_>)?\ = <l>entry\(^TreeMapEntry#C<K#G, ^_>)?\);
        return ret;
    }
    public boolean hasNext() {
        return ERROR: Unknown (<MTreeMapKeyIterator#C<K#G>>self.<eIm>entry\(^TreeMapEntry#C<§K#G§, ^_>)?\ != none<^TreeMapEntry#C<§K#G§, ^_>>);
    }
    public K next() {
        ERROR: Unknown local ret : K#G = <MTreeMapKeyIterator#C<K#G>>self.<eIm>entry\(^TreeMapEntry#C<§K#G§, ^_>)?\.get.<eIUm>key\§K#G§\;
        ERROR: Unknown (<MTreeMapKeyIterator#C<K#G>>self.<emp>prev\(^TreeMapEntry#C<§K#G§, ^_>)?\ = <MTreeMapKeyIterator#C<K#G>>self.<eIm>entry\(^TreeMapEntry#C<§K#G§, ^_>)?\);
        ERROR: Unknown (<MTreeMapKeyIterator#C<K#G>>self.<eIm>entry\(^TreeMapEntry#C<§K#G§, ^_>)?\ = <MTreeMapKeyIterator#C<K#G>>self.<eIm>entry\(^TreeMapEntry#C<§K#G§, ^_>)?\.get.<dI>next\(^TreeMapEntry#C<§K#G§, ^_>)?\);
        return ret;
    }
    public void remove() {
        {
            ERROR: Unknown local _ : ^(^TreeMapEntry#C<§K#G§, ^_>)¿ = <MTreeMapKeyIterator#C<K#G>>self.<emp>prev\(^TreeMapEntry#C<§K#G§, ^_>)?\;
            ERROR: Unknown if((<l>_\^(^TreeMapEntry#C<§K#G§, ^_>)¿\ != none<^TreeMapEntry#C<§K#G§, ^_>>)) <MTreeMapKeyIterator#C<K#G>>self.<eIU>map\MTreeMap#C<§K#G§, ^_>\.<dp>delete(entry = <l>_\^(^TreeMapEntry#C<§K#G§, ^_>)¿\)\^_\;
        }
    }
    public void setValue(K value) {
        {
            ERROR: Unknown local p : ^(^TreeMapEntry#C<§K#G§, ^_>)¿ = <MTreeMapKeyIterator#C<K#G>>self.<emp>prev\(^TreeMapEntry#C<§K#G§, ^_>)?\;
            ERROR: Unknown if((<l>p\^(^TreeMapEntry#C<§K#G§, ^_>)¿\ != none<^TreeMapEntry#C<§K#G§, ^_>>)) if((<l>p\^(^TreeMapEntry#C<§K#G§, ^_>)¿\.<eIUm>key\§K#G§\ != <l>value\§K#G§\)) {
    <MTreeMapKeyIterator#C<K#G>>self.<eIU>map\MTreeMap#C<§K#G§, ^_>\.<dp>delete(entry = <l>p\^(^TreeMapEntry#C<§K#G§, ^_>)¿\)\^_\
    <MTreeMapKeyIterator#C<K#G>>self.<eIU>map\MTreeMap#C<§K#G§, ^_>\.<dIo>set(key = <l>value\§K#G§\, value = <l>p\^(^TreeMapEntry#C<§K#G§, ^_>)¿\.<eIUm>value\^_\)\void\
};
        }
    }
    public MTreeMapKeyIterator(MTreeMap<K, Object> map) {
    }
    static final ClassType<MTreeMapKeyIterator<K>> type;
}