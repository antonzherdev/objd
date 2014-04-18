package core.chain;

public class MTreeMapIterator<K, V> implements MIterator<Tuple2<K, V>> {
    public final MTreeMap<K, V> map;
    private TreeMapEntry<K, V> prev;
    public TreeMapEntry<K, V> entry;
    public static MTreeMapIterator<K, V> applyMapEntry(MTreeMap<K, V> map,TreeMapEntry<K, V> entry) {
        ERROR: Unknown local ret : MTreeMapIterator#C<§K#G§, §V#G§> = <to>MTreeMapIterator\MTreeMapIterator#C.class\.<tcI>apply(map = <l>map\MTreeMap#C<K#G, V#G>\)\MTreeMapIterator#C<§K#G§, §V#G§>\;
        ERROR: Unknown (<l>ret\MTreeMapIterator#C<§K#G§, §V#G§>\.<eIm>entry\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ = <l>entry\(^TreeMapEntry#C<K#G, V#G>)?\);
        return ERROR: Unknown <l>ret\MTreeMapIterator#C<§K#G§, §V#G§>\;
    }
    public boolean hasNext() {
        return ERROR: Unknown (<MTreeMapIterator#C<K#G, V#G>>self.<eIm>entry\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>);
    }
    public Tuple2<K, V> next() {
        ERROR: Unknown local ret : (§K#G§, §V#G§) = (<MTreeMapIterator#C<K#G, V#G>>self.<eIm>entry\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIUm>key\§K#G§\, <MTreeMapIterator#C<K#G, V#G>>self.<eIm>entry\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIUm>value\§V#G§\);
        ERROR: Unknown (<MTreeMapIterator#C<K#G, V#G>>self.<emp>prev\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ = <MTreeMapIterator#C<K#G, V#G>>self.<eIm>entry\(^TreeMapEntry#C<§K#G§, §V#G§>)?\);
        ERROR: Unknown (<MTreeMapIterator#C<K#G, V#G>>self.<eIm>entry\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ = <MTreeMapIterator#C<K#G, V#G>>self.<eIm>entry\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<dI>next\(^TreeMapEntry#C<§K#G§, §V#G§>)?\);
        return ERROR: Unknown <l>ret\(§K#G§, §V#G§)\;
    }
    public void remove() {
        {
            ERROR: Unknown local _ : ^(^TreeMapEntry#C<§K#G§, §V#G§>)¿ = <MTreeMapIterator#C<K#G, V#G>>self.<emp>prev\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
            ERROR: Unknown if((<l>_\^(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>)) <MTreeMapIterator#C<K#G, V#G>>self.<eIU>map\MTreeMap#C<§K#G§, §V#G§>\.<dp>delete(entry = <l>_\^(^TreeMapEntry#C<§K#G§, §V#G§>)¿\)\§V#G§\;
        }
    }
    public void setValue(Tuple2<K, V> value) {
        {
            ERROR: Unknown local p : ^(^TreeMapEntry#C<§K#G§, §V#G§>)¿ = <MTreeMapIterator#C<K#G, V#G>>self.<emp>prev\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
            ERROR: Unknown if((<l>p\^(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>)) if((<l>p\^(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUm>key\§K#G§\ == <l>value\^(§K#G§, §V#G§)\.<eIU>a\§K#G§\)) (<l>p\^(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUm>value\§V#G§\ = <l>value\^(§K#G§, §V#G§)\)
else {
    <MTreeMapIterator#C<K#G, V#G>>self.<eIU>map\MTreeMap#C<§K#G§, §V#G§>\.<dp>delete(entry = <l>p\^(^TreeMapEntry#C<§K#G§, §V#G§>)¿\)\§V#G§\
    <MTreeMapIterator#C<K#G, V#G>>self.<eIU>map\MTreeMap#C<§K#G§, §V#G§>\.<dIo>set(key = <l>value\^(§K#G§, §V#G§)\.<eIU>a\§K#G§\, value = <l>value\^(§K#G§, §V#G§)\.<eIU>b\§V#G§\)\void\
};
        }
    }
    public MTreeMapIterator(MTreeMap<K, V> map) {
    }
    static final ClassType<MTreeMapIterator<K, V>> type;
}