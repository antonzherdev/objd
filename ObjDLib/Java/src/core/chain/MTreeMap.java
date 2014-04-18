package core.chain;

public class MTreeMap<K, V> extends TreeMap<K, V> implements MMap<K, V> {
    private TreeMapEntry<K, V> _root = ERROR: Unknown none<^TreeMapEntry#C<K#G, V#G>>;
    private int _size = ERROR: Unknown 0.cast<uint>;
    public final MTreeMapKeySet<K> keys = new MTreeMapKeySet<K>(ERROR: Unknown <MTreeMap#C<K#G, V#G>>self);
    public static MTreeMap<K, V> apply() {
        return new MTreeMap<K, V>(ERROR: Unknown a : K#G, b : K#G -> int = weak return <l>a\K#G\.<rdI>compare(to = <l>b\K#G\)\int\);
    }
    public ImTreeMap<K, V> imCopy() {
        return new ImTreeMap<K, V>(comparator, ERROR: Unknown <MTreeMap#C<K#G, V#G>>self.<emp>_root\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<dI>copy(parent = none<^TreeMapEntry#C<§K#G§, §V#G§>>)\TreeMapEntry#C<§K#G§, §V#G§>\, _size);
    }
    public ImTreeMap<K, V> im() {
        return new ImTreeMap<K, V>(comparator, _root, _size);
    }
    public void assignImMap(ImMap<K, V> imMap) {
        ERROR: Unknown if(<l>imMap\ImMap#T<§K#G§, §V#G§>\.is<ImTreeMap#C<K#G, V#G>>) {
    local m : ImTreeMap#C<K#G, V#G> = <l>imMap\ImMap#T<§K#G§, §V#G§>\.cast<ImTreeMap#C<K#G, V#G>>
    (<MTreeMap#C<K#G, V#G>>self.<emp>_root\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ = <l>m\ImTreeMap#C<§K#G§, §V#G§>\.<eIUo>root\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<dI>copy(parent = none<^TreeMapEntry#C<§K#G§, §V#G§>>)\TreeMapEntry#C<§K#G§, §V#G§>\)
    (<MTreeMap#C<K#G, V#G>>self.<emp>_size\uint\ = <l>m\ImTreeMap#C<§K#G§, §V#G§>\.<eIUo>count\uint\)
}
else {
    <MTreeMap#C<K#G, V#G>>self.<dIo>clear\void\
    <l>imMap\ImMap#T<§K#G§, §V#G§>\.<rdIo>for(each = _ : ^(§K#G§, §V#G§) -> void = <MTreeMap#C<K#G, V#G>>self.<rdIo>append(item = <l>_\^(§K#G§, §V#G§)\)\void\)\void\
};
    }
    public TreeMapEntry<K, V> root() {
        return _root;
    }
    public int count() {
        return _size;
    }
    public void clear() {
        ERROR: Unknown (<MTreeMap#C<K#G, V#G>>self.<emp>_size\uint\ = 0.cast<uint>);
        ERROR: Unknown (<MTreeMap#C<K#G, V#G>>self.<emp>_root\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ = none<^TreeMapEntry#C<§K#G§, §V#G§>>);
    }
    public MIterator<Tuple2<K, V>> mutableIterator() {
        return MTreeMapIterator().applyMapEntry<K, V>(ERROR: Unknown <MTreeMap#C<K#G, V#G>>self, firstEntry());
    }
    public void setKeyValue(K key,V value) {
        ERROR: Unknown local __comparator : (§K#G§, §K#G§) -> int = <MTreeMap#C<K#G, V#G>>self.<reIU>comparator\(§K#G§, §K#G§) -> int\;
        ERROR: Unknown local var t : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <MTreeMap#C<K#G, V#G>>self.<emp>_root\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
        ERROR: Unknown if((<lm>t\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == none<^TreeMapEntry#C<§K#G§, §V#G§>>)) {
    (<MTreeMap#C<K#G, V#G>>self.<emp>_root\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ = some(<to>TreeMapEntry\TreeMapEntry#C.class\.<tcI>apply(key = <l>key\§K#G§\, value = <l>value\§V#G§\, parent = none<^TreeMapEntry#C<§K#G§, §V#G§>>)\TreeMapEntry#C<§K#G§, §V#G§>\)\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
    (<MTreeMap#C<K#G, V#G>>self.<emp>_size\uint\ = 1.cast<uint>)
}
else {
    local var cmp : int = 0
    local var parent : (^TreeMapEntry#C<K#G, V#G>)? = none<^TreeMapEntry#C<K#G, V#G>>
    do{
    (<lm>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ = <lm>t\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.cast<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)
    (<lm>cmp\int\ = <l>__comparator\(§K#G§, §K#G§) -> int\.<d>apply( = <l>key\§K#G§\,  = <lm>t\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUm>key\§K#G§\)\int\)
    if((<lm>cmp\int\ < 0)) (<lm>t\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>t\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
else if((<lm>cmp\int\ > 0)) (<lm>t\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>t\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
else {
    (<lm>t\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUm>value\§V#G§\ = <l>value\§V#G§\)
    return nil
}
} while((<lm>t\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>))
    local e : TreeMapEntry#C<§K#G§, §V#G§> = <to>TreeMapEntry\TreeMapEntry#C.class\.<tcI>apply(key = <l>key\§K#G§\, value = <l>value\§V#G§\, parent = <lm>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)\TreeMapEntry#C<§K#G§, §V#G§>\
    if((<lm>cmp\int\ < 0)) (<lm>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ = some(<l>e\TreeMapEntry#C<§K#G§, §V#G§>\)\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
else (<lm>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ = some(<l>e\TreeMapEntry#C<§K#G§, §V#G§>\)\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
    <MTreeMap#C<K#G, V#G>>self.<dp>fixAfterInsertion(entry = <l>e\TreeMapEntry#C<§K#G§, §V#G§>\)\void\
    <MTreeMap#C<K#G, V#G>>self.<emp>_size\uint\++
};
    }
    public V removeForKey(K key) {
        ERROR: Unknown local _ : ^(^TreeMapEntry#C<§K#G§, §V#G§>)¿ = <MTreeMap#C<K#G, V#G>>self.<rdI>entryFor(key = <l>key\§K#G§\)\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
        ERROR: Unknown if((<l>_\^(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>)) return some(<MTreeMap#C<K#G, V#G>>self.<dp>delete(entry = <l>_\^(^TreeMapEntry#C<§K#G§, §V#G§>)¿\)\§V#G§\)\§(V#G)?§\
else return some(none<^void>)\^void\;
    }
    private V deleteEntry(TreeMapEntry<K, V> entry) {
        ERROR: Unknown local var p : TreeMapEntry#C<§K#G§, §V#G§> = <l>entry\TreeMapEntry#C<§K#G§, §V#G§>\;
        ERROR: Unknown <MTreeMap#C<K#G, V#G>>self.<emp>_size\uint\--;
        ERROR: Unknown if(((<lm>p\TreeMapEntry#C<§K#G§, §V#G§>\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>) && (<lm>p\TreeMapEntry#C<§K#G§, §V#G§>\.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>))) {
    local s : TreeMapEntry#C<§K#G§, §V#G§> = <lm>p\TreeMapEntry#C<§K#G§, §V#G§>\.<dI>next\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get
    (<lm>p\TreeMapEntry#C<§K#G§, §V#G§>\.<eIUm>key\§K#G§\ = <l>s\TreeMapEntry#C<§K#G§, §V#G§>\.<eIUm>key\§K#G§\)
    (<lm>p\TreeMapEntry#C<§K#G§, §V#G§>\.<eIUm>value\§V#G§\ = <l>s\TreeMapEntry#C<§K#G§, §V#G§>\.<eIUm>value\§V#G§\)
    (<lm>p\TreeMapEntry#C<§K#G§, §V#G§>\ = <l>s\TreeMapEntry#C<§K#G§, §V#G§>\)
};
        ERROR: Unknown local replacement : (^TreeMapEntry#C<§K#G§, §V#G§>)? = if((<lm>p\TreeMapEntry#C<§K#G§, §V#G§>\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>)) <lm>p\TreeMapEntry#C<§K#G§, §V#G§>\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
else <lm>p\TreeMapEntry#C<§K#G§, §V#G§>\.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
        ERROR: Unknown if((<l>replacement\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>)) {
    (<l>replacement\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ = <lm>p\TreeMapEntry#C<§K#G§, §V#G§>\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
    if((<lm>p\TreeMapEntry#C<§K#G§, §V#G§>\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == none<^TreeMapEntry#C<§K#G§, §V#G§>>)) (<MTreeMap#C<K#G, V#G>>self.<emp>_root\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ = <l>replacement\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.cast<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)
else if({
    local __tmp_4_1 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>p\TreeMapEntry#C<§K#G§, §V#G§>\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    return ((<l>__tmp_4_1\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>) && (<l>__tmp_4_1\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == <lm>p\TreeMapEntry#C<§K#G§, §V#G§>\))
}) (<lm>p\TreeMapEntry#C<§K#G§, §V#G§>\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ = <l>replacement\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.cast<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)
else (<lm>p\TreeMapEntry#C<§K#G§, §V#G§>\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ = <l>replacement\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.cast<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)
    (<lm>p\TreeMapEntry#C<§K#G§, §V#G§>\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ = none<^TreeMapEntry#C<§K#G§, §V#G§>>)
    (<lm>p\TreeMapEntry#C<§K#G§, §V#G§>\.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ = none<^TreeMapEntry#C<§K#G§, §V#G§>>)
    (<lm>p\TreeMapEntry#C<§K#G§, §V#G§>\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ = none<^TreeMapEntry#C<§K#G§, §V#G§>>)
    if((<lm>p\TreeMapEntry#C<§K#G§, §V#G§>\.<eIm>color\int\ == <MTreeMap#C<K#G, V#G>>self.<reIt>BLACK\int\)) <MTreeMap#C<K#G, V#G>>self.<dp>fixAfterDeletion(entry = <l>replacement\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\)\void\
}
else if((<lm>p\TreeMapEntry#C<§K#G§, §V#G§>\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == none<^TreeMapEntry#C<§K#G§, §V#G§>>)) {
    (<MTreeMap#C<K#G, V#G>>self.<emp>_root\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ = none<^TreeMapEntry#C<§K#G§, §V#G§>>)
}
else {
    if((<lm>p\TreeMapEntry#C<§K#G§, §V#G§>\.<eIm>color\int\ == <MTreeMap#C<K#G, V#G>>self.<reIt>BLACK\int\)) <MTreeMap#C<K#G, V#G>>self.<dp>fixAfterDeletion(entry = <lm>p\TreeMapEntry#C<§K#G§, §V#G§>\)\void\
    if((<lm>p\TreeMapEntry#C<§K#G§, §V#G§>\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>)) {
    if({
    local __tmp_4_1_0 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>p\TreeMapEntry#C<§K#G§, §V#G§>\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    return ((<l>__tmp_4_1_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>) && (<l>__tmp_4_1_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == <lm>p\TreeMapEntry#C<§K#G§, §V#G§>\))
}) (<lm>p\TreeMapEntry#C<§K#G§, §V#G§>\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ = none<^TreeMapEntry#C<§K#G§, §V#G§>>)
else if({
    local __tmp_4_1_0 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>p\TreeMapEntry#C<§K#G§, §V#G§>\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    return ((<l>__tmp_4_1_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>) && (<l>__tmp_4_1_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == <lm>p\TreeMapEntry#C<§K#G§, §V#G§>\))
}) (<lm>p\TreeMapEntry#C<§K#G§, §V#G§>\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ = none<^TreeMapEntry#C<§K#G§, §V#G§>>)
    (<lm>p\TreeMapEntry#C<§K#G§, §V#G§>\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ = none<^TreeMapEntry#C<§K#G§, §V#G§>>)
}
};
        return entry.value;
    }
    private void fixAfterInsertionEntry(TreeMapEntry<K, V> entry) {
        ERROR: Unknown (<l>entry\TreeMapEntry#C<§K#G§, §V#G§>\.<eIm>color\int\ = <MTreeMap#C<K#G, V#G>>self.<reIt>RED\int\);
        ERROR: Unknown local var x : (^TreeMapEntry#C<K#G, V#G>)? = some(<l>entry\TreeMapEntry#C<§K#G§, §V#G§>\)\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
        ERROR: Unknown while((((<lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>) && ((<MTreeMap#C<K#G, V#G>>self.<emp>_root\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == none<^TreeMapEntry#C<§K#G§, §V#G§>>) || (<MTreeMap#C<K#G, V#G>>self.<emp>_root\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\))) && (<lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>color\int\ == <MTreeMap#C<K#G, V#G>>self.<reIt>RED\int\))) {
    if({
    local __tmp_2_0_l : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    local __tmp_2_0_r : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    return ((<l>__tmp_2_0_l\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ === <l>__tmp_2_0_r\(^TreeMapEntry#C<§K#G§, §V#G§>)?\) || (((<l>__tmp_2_0_l\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>) && (<l>__tmp_2_0_r\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>)) && (<l>__tmp_2_0_l\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == <l>__tmp_2_0_r\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)))
}) {
    local y : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if(((<l>y\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>) && (<l>y\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>color\int\ == <MTreeMap#C<K#G, V#G>>self.<reIt>RED\int\))) {
    {
    local __tmp_2_0_1_0 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_2_0_1_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) (<l>__tmp_2_0_1_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>color\int\ = <MTreeMap#C<K#G, V#G>>self.<reIt>BLACK\int\)
}
    (<l>y\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>color\int\ = <MTreeMap#C<K#G, V#G>>self.<reIt>BLACK\int\)
    {
    local __tmp_2_0_1_2 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_2_0_1_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) (<l>__tmp_2_0_1_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>color\int\ = <MTreeMap#C<K#G, V#G>>self.<reIt>RED\int\)
}
    (<lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
}
else {
    if({
    local __tmp_2_0_1_0 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    return ((<l>__tmp_2_0_1_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>) && (<l>__tmp_2_0_1_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\))
}) {
    (<lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
    <MTreeMap#C<K#G, V#G>>self.<dp>rotateLeft(p = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)\void\
}
    {
    local __tmp_2_0_1_1 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_2_0_1_1\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) (<l>__tmp_2_0_1_1\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>color\int\ = <MTreeMap#C<K#G, V#G>>self.<reIt>BLACK\int\)
}
    {
    local __tmp_2_0_1_2 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_2_0_1_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) (<l>__tmp_2_0_1_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>color\int\ = <MTreeMap#C<K#G, V#G>>self.<reIt>RED\int\)
}
    <MTreeMap#C<K#G, V#G>>self.<dp>rotateRight(p = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)\void\
}
}
else {
    local y : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if(((<l>y\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>) && (<l>y\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>color\int\ == <MTreeMap#C<K#G, V#G>>self.<reIt>RED\int\))) {
    {
    local __tmp_2_0_1_0 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_2_0_1_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) (<l>__tmp_2_0_1_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>color\int\ = <MTreeMap#C<K#G, V#G>>self.<reIt>BLACK\int\)
}
    (<l>y\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>color\int\ = <MTreeMap#C<K#G, V#G>>self.<reIt>BLACK\int\)
    {
    local __tmp_2_0_1_2 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_2_0_1_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) (<l>__tmp_2_0_1_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>color\int\ = <MTreeMap#C<K#G, V#G>>self.<reIt>RED\int\)
}
    (<lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
}
else {
    if({
    local __tmp_2_0_1_0 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    return ((<l>__tmp_2_0_1_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>) && (<l>__tmp_2_0_1_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\))
}) {
    (<lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
    <MTreeMap#C<K#G, V#G>>self.<dp>rotateRight(p = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)\void\
}
    {
    local __tmp_2_0_1_1 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_2_0_1_1\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) (<l>__tmp_2_0_1_1\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>color\int\ = <MTreeMap#C<K#G, V#G>>self.<reIt>BLACK\int\)
}
    {
    local __tmp_2_0_1_2 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_2_0_1_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) (<l>__tmp_2_0_1_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>color\int\ = <MTreeMap#C<K#G, V#G>>self.<reIt>RED\int\)
}
    <MTreeMap#C<K#G, V#G>>self.<dp>rotateLeft(p = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)\void\
}
}
};
        ERROR: Unknown if((<MTreeMap#C<K#G, V#G>>self.<emp>_root\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) (<MTreeMap#C<K#G, V#G>>self.<emp>_root\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>color\int\ = <MTreeMap#C<K#G, V#G>>self.<reIt>BLACK\int\);
    }
    private void fixAfterDeletionEntry(TreeMapEntry<K, V> entry) {
        ERROR: Unknown local var x : (^TreeMapEntry#C<K#G, V#G>)? = some(<l>entry\TreeMapEntry#C<§K#G§, §V#G§>\)\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
        ERROR: Unknown while((((<lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>) && ((<MTreeMap#C<K#G, V#G>>self.<emp>_root\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == none<^TreeMapEntry#C<§K#G§, §V#G§>>) || (<MTreeMap#C<K#G, V#G>>self.<emp>_root\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\))) && (<lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>color\int\ == <MTreeMap#C<K#G, V#G>>self.<reIt>BLACK\int\))) {
    if({
    local __tmp_1_0 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    return ((<l>__tmp_1_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>) && (<l>__tmp_1_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\))
}) {
    local var sib : (^TreeMapEntry#C<K#G, V#G>)? = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if(((<lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>) && (<lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>color\int\ == <MTreeMap#C<K#G, V#G>>self.<reIt>RED\int\))) {
    (<lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>color\int\ = <MTreeMap#C<K#G, V#G>>self.<reIt>BLACK\int\)
    {
    local __tmp_1_0_1_1 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_1_0_1_1\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) (<l>__tmp_1_0_1_1\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>color\int\ = <MTreeMap#C<K#G, V#G>>self.<reIt>RED\int\)
}
    <MTreeMap#C<K#G, V#G>>self.<dp>rotateLeft(p = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)\void\
    (<lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
}
    if((({
    local __tmp_1_0_2 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_1_0_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>color\int\
else <MTreeMap#C<K#G, V#G>>self.<reIt>BLACK\int\
} == <MTreeMap#C<K#G, V#G>>self.<reIt>BLACK\int\) && ({
    local __tmp_1_0_2 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_1_0_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>color\int\
else <MTreeMap#C<K#G, V#G>>self.<reIt>BLACK\int\
} == <MTreeMap#C<K#G, V#G>>self.<reIt>BLACK\int\))) {
    if((<lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) (<lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>color\int\ = <MTreeMap#C<K#G, V#G>>self.<reIt>RED\int\)
    (<lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
}
else {
    if(({
    local __tmp_1_0_2_0 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_1_0_2_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>color\int\
else <MTreeMap#C<K#G, V#G>>self.<reIt>BLACK\int\
} == <MTreeMap#C<K#G, V#G>>self.<reIt>BLACK\int\)) {
    {
    local __tmp_1_0_2_0_0 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_1_0_2_0_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) (<l>__tmp_1_0_2_0_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>color\int\ = <MTreeMap#C<K#G, V#G>>self.<reIt>BLACK\int\)
}
    if((<lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) (<lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>color\int\ = <MTreeMap#C<K#G, V#G>>self.<reIt>RED\int\)
    <MTreeMap#C<K#G, V#G>>self.<dp>rotateRight(p = <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)\void\
    (<lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
}
    if((<lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) (<lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>color\int\ = {
    local __tmp_1_0_2_1 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_1_0_2_1\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) return <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>color\int\
else return <MTreeMap#C<K#G, V#G>>self.<reIt>BLACK\int\
})
    {
    local __tmp_1_0_2_2 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_1_0_2_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) (<l>__tmp_1_0_2_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>color\int\ = <MTreeMap#C<K#G, V#G>>self.<reIt>BLACK\int\)
}
    {
    local __tmp_1_0_2_3 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_1_0_2_3\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) (<l>__tmp_1_0_2_3\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>color\int\ = <MTreeMap#C<K#G, V#G>>self.<reIt>BLACK\int\)
}
    <MTreeMap#C<K#G, V#G>>self.<dp>rotateLeft(p = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)\void\
    (<lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <MTreeMap#C<K#G, V#G>>self.<emp>_root\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
}
}
else {
    local var sib : (^TreeMapEntry#C<K#G, V#G>)? = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if(((<lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>) && (<lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>color\int\ == <MTreeMap#C<K#G, V#G>>self.<reIt>RED\int\))) {
    (<lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>color\int\ = <MTreeMap#C<K#G, V#G>>self.<reIt>BLACK\int\)
    {
    local __tmp_1_0_1_1 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_1_0_1_1\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) (<l>__tmp_1_0_1_1\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>color\int\ = <MTreeMap#C<K#G, V#G>>self.<reIt>RED\int\)
}
    <MTreeMap#C<K#G, V#G>>self.<dp>rotateRight(p = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)\void\
    (<lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
}
    if((({
    local __tmp_1_0_2 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_1_0_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>color\int\
else <MTreeMap#C<K#G, V#G>>self.<reIt>BLACK\int\
} == <MTreeMap#C<K#G, V#G>>self.<reIt>BLACK\int\) && ({
    local __tmp_1_0_2 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_1_0_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>color\int\
else <MTreeMap#C<K#G, V#G>>self.<reIt>BLACK\int\
} == <MTreeMap#C<K#G, V#G>>self.<reIt>BLACK\int\))) {
    if((<lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) (<lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>color\int\ = <MTreeMap#C<K#G, V#G>>self.<reIt>RED\int\)
    (<lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
}
else {
    if(({
    local __tmp_1_0_2_0 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_1_0_2_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>color\int\
else <MTreeMap#C<K#G, V#G>>self.<reIt>BLACK\int\
} == <MTreeMap#C<K#G, V#G>>self.<reIt>BLACK\int\)) {
    {
    local __tmp_1_0_2_0_0 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_1_0_2_0_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) (<l>__tmp_1_0_2_0_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>color\int\ = <MTreeMap#C<K#G, V#G>>self.<reIt>BLACK\int\)
}
    if((<lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) (<lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>color\int\ = <MTreeMap#C<K#G, V#G>>self.<reIt>RED\int\)
    <MTreeMap#C<K#G, V#G>>self.<dp>rotateLeft(p = <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)\void\
    (<lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
}
    if((<lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) (<lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>color\int\ = {
    local __tmp_1_0_2_1 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_1_0_2_1\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) return <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>color\int\
else return <MTreeMap#C<K#G, V#G>>self.<reIt>BLACK\int\
})
    {
    local __tmp_1_0_2_2 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_1_0_2_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) (<l>__tmp_1_0_2_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>color\int\ = <MTreeMap#C<K#G, V#G>>self.<reIt>BLACK\int\)
}
    {
    local __tmp_1_0_2_3 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_1_0_2_3\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) (<l>__tmp_1_0_2_3\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>color\int\ = <MTreeMap#C<K#G, V#G>>self.<reIt>BLACK\int\)
}
    <MTreeMap#C<K#G, V#G>>self.<dp>rotateRight(p = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)\void\
    (<lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <MTreeMap#C<K#G, V#G>>self.<emp>_root\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
}
}
};
        ERROR: Unknown if((<lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) (<lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>color\int\ = <MTreeMap#C<K#G, V#G>>self.<reIt>BLACK\int\);
    }
    private void rotateLeftP(TreeMapEntry<K, V> p) {
        ERROR: Unknown if((<l>p\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>)) {
    local r : TreeMapEntry#C<§K#G§, §V#G§> = <l>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get
    (<l>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ = <l>r\TreeMapEntry#C<§K#G§, §V#G§>\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
    {
    local __tmp_0_2 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <l>r\TreeMapEntry#C<§K#G§, §V#G§>\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_0_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) (<l>__tmp_0_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ = <l>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.cast<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)
}
    (<l>r\TreeMapEntry#C<§K#G§, §V#G§>\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ = <l>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
    if((<l>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == none<^TreeMapEntry#C<§K#G§, §V#G§>>)) (<MTreeMap#C<K#G, V#G>>self.<emp>_root\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ = some(<l>r\TreeMapEntry#C<§K#G§, §V#G§>\)\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
else if({
    local __tmp_0_4 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <l>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    return ((<l>__tmp_0_4\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>) && (<l>__tmp_0_4\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == <l>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\))
}) (<l>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ = some(<l>r\TreeMapEntry#C<§K#G§, §V#G§>\)\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
else (<l>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ = some(<l>r\TreeMapEntry#C<§K#G§, §V#G§>\)\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
    (<l>r\TreeMapEntry#C<§K#G§, §V#G§>\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ = <l>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.cast<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)
    (<l>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ = some(<l>r\TreeMapEntry#C<§K#G§, §V#G§>\)\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
};
    }
    private void rotateRightP(TreeMapEntry<K, V> p) {
        ERROR: Unknown if((<l>p\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>)) {
    local l : TreeMapEntry#C<§K#G§, §V#G§> = <l>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get
    (<l>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ = <l>l\TreeMapEntry#C<§K#G§, §V#G§>\.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
    {
    local __tmp_0_2 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <l>l\TreeMapEntry#C<§K#G§, §V#G§>\.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_0_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) (<l>__tmp_0_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ = <l>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.cast<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)
}
    (<l>l\TreeMapEntry#C<§K#G§, §V#G§>\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ = <l>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
    if((<l>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == none<^TreeMapEntry#C<§K#G§, §V#G§>>)) (<MTreeMap#C<K#G, V#G>>self.<emp>_root\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ = some(<l>l\TreeMapEntry#C<§K#G§, §V#G§>\)\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
else if({
    local __tmp_0_4 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <l>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    return ((<l>__tmp_0_4\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>) && (<l>__tmp_0_4\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == <l>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\))
}) (<l>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ = some(<l>l\TreeMapEntry#C<§K#G§, §V#G§>\)\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
else (<l>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ = some(<l>l\TreeMapEntry#C<§K#G§, §V#G§>\)\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
    (<l>l\TreeMapEntry#C<§K#G§, §V#G§>\.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ = <l>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.cast<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)
    (<l>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ = some(<l>l\TreeMapEntry#C<§K#G§, §V#G§>\)\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
};
    }
    public Tuple2<K, V> pollFirst() {
        ERROR: Unknown local entry : ^(^TreeMapEntry#C<§K#G§, §V#G§>)¿ = <MTreeMap#C<K#G, V#G>>self.<rdI>firstEntry\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
        ERROR: Unknown if((<l>entry\^(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>)) {
    <MTreeMap#C<K#G, V#G>>self.<dp>delete(entry = <l>entry\^(^TreeMapEntry#C<§K#G§, §V#G§>)¿\)\§V#G§\
    return some((<l>entry\^(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUm>key\§K#G§\, <l>entry\^(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUm>value\§V#G§\))\(^(§K#G§, §V#G§))?\
}
else return none<^(§K#G§, §V#G§)>;
    }
    public MTreeMap(F2<K, K, Integer> comparator) {
    }
    static final ClassType<MTreeMap<K, V>> type;
    public V objectForKeyOrUpdateWith(K key,F<Void, V> orUpdateWith) {
        ERROR: Unknown local __tmp : §(V#G)?§ = <MMap#T<K#G, V#G>>self.<rdIa>opt(key = <l>key\§K#G§\)\(§V#G§)?\;
        ERROR: Unknown if((<l>__tmp\§(V#G)?§\ != none<§V#G§>)) return <l>__tmp\§(V#G)?§\.get
else {
    local init : V#G = <l>orUpdateWith\void -> §V#G§\()
    <MMap#T<K#G, V#G>>self.<dIa>set(key = <l>key\§K#G§\, value = <l>init\§V#G§\)\void\
    return <l>init\§V#G§\
};
    }
    public V modifyKeyBy(K key,F<V, V> by) {
        ERROR: Unknown local newObject : (§V#G§)? = <l>by\(§V#G§)? -> (§V#G§)?\.<d>apply( = <MMap#T<K#G, V#G>>self.<rdIa>opt(key = <l>key\§K#G§\)\(§V#G§)?\)\(§V#G§)?\;
        ERROR: Unknown if((<l>newObject\(§V#G§)?\ == none<§V#G§>)) <MMap#T<K#G, V#G>>self.<dIa>removeFor(key = <l>key\§K#G§\)\(§V#G§)?\
else <MMap#T<K#G, V#G>>self.<dIa>set(key = <l>key\§K#G§\, value = <l>newObject\(§V#G§)¿\)\void\;
        return newObject;
    }
    public V takeKey(K key) {
        ERROR: Unknown local ret : (§V#G§)? = <MMap#T<K#G, V#G>>self.<rdIa>opt(key = <l>key\§K#G§\)\(§V#G§)?\;
        removeForKey(key);
        return ret;
    }
    public void appendItem(Tuple2<K, V> item) {
        setKeyValue(item.b, item.a);
    }
    public boolean removeItem(Tuple2<K, V> item) {
        return ERROR: Unknown (<MMap#T<K#G, V#G>>self.<dIa>removeFor(key = <l>item\^(§K#G§, §V#G§)\.<eIU>a\§K#G§\)\(§V#G§)?\ != none<§V#G§>);
    }
    public boolean removeItem(T item) {
        ERROR: Unknown local i : MIterator#T<§T#G§> = <MIterable#T<T#G>>self.<dIa>mutableIterator\MIterator#T<§T#G§>\;
        ERROR: Unknown local var ret : bool = False;
        ERROR: Unknown while(<l>i\MIterator#T<§T#G§>\.<rdIa>hasNext\bool\) {
    if((<l>i\MIterator#T<§T#G§>\.<rdIa>next\§T#G§\ == <l>item\§T#G§\)) {
    <l>i\MIterator#T<§T#G§>\.<dIa>remove\void\
    (<lm>ret\bool\ = True)
}
};
        return ret;
    }
    public void mutableFilterBy(F<T, Boolean> by) {
        ERROR: Unknown local i : MIterator#T<§T#G§> = <MIterable#T<T#G>>self.<dIa>mutableIterator\MIterator#T<§T#G§>\;
        ERROR: Unknown while(<l>i\MIterator#T<§T#G§>\.<rdIa>hasNext\bool\) {
    if(<l>by\§T#G§ -> bool\.<d>apply( = <l>i\MIterator#T<§T#G§>\.<rdIa>next\§T#G§\)\bool\) <l>i\MIterator#T<§T#G§>\.<dIa>remove\void\
};
    }
}