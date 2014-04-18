package core.chain;

public class MTreeMap<K, V> extends TreeMap<K, V> implements MMap<K, V> {
    private TreeMapEntry<K, V> _root = null;
    private int _size = ERROR: Unknown 0.cast<uint>;
    public final MTreeMapKeySet<K> keys = new MTreeMapKeySet<K>(this);
    public static MTreeMap<K, V> apply() {
        return new MTreeMap<K, V>(new F2<K, K, Integer>() {
            @Override
            public Integer apply(K a,K b) {
                ERROR: Unknown weak return <l>a\K#G\.<rdI>compare(to = <l>b\K#G\)\int\;
            }
        });
    }
    @Override
    public ImTreeMap<K, V> imCopy() {
        return new ImTreeMap<K, V>(this.comparator, ERROR: Unknown <MTreeMap#C<K#G, V#G>>self.<emp>_root\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<dI>copy(parent = none<^TreeMapEntry#C<§K#G§, §V#G§>>)\TreeMapEntry#C<§K#G§, §V#G§>\, this._size);
    }
    @Override
    public ImTreeMap<K, V> im() {
        return new ImTreeMap<K, V>(this.comparator, this._root, this._size);
    }
    @Override
    public void assignImMap(ImMap<K, V> imMap) {
        if(imMap.ERROR: Unknown is<ImTreeMap#C<K#G, V#G>>) {
            ERROR: Unknown local m : ImTreeMap#C<K#G, V#G> = <l>imMap\ImMap#T<§K#G§, §V#G§>\.cast<ImTreeMap#C<K#G, V#G>>;
            this._root = ERROR: Unknown <l>m\ImTreeMap#C<§K#G§, §V#G§>\.<eIUo>root\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<dI>copy(parent = none<^TreeMapEntry#C<§K#G§, §V#G§>>)\TreeMapEntry#C<§K#G§, §V#G§>\;
            this._size = m.count;
        } else {
            this.clear();
            imMap.forEach(new P<Tuple2<K, V>>() {
                @Override
                public void apply(Tuple2<K, V> _) {
                    appendItem(_);
                }
            });
        }
    }
    @Override
    public TreeMapEntry<K, V> root() {
        return this._root;
    }
    @Override
    public int count() {
        return this._size;
    }
    @Override
    public void clear() {
        this._size = ERROR: Unknown 0.cast<uint>;
        this._root = null;
    }
    @Override
    public MIterator<Tuple2<K, V>> mutableIterator() {
        return MTreeMapIterator().applyMapEntry<K, V>(this, this.firstEntry());
    }
    @Override
    public void setKeyValue(K key,V value) {
        ERROR: Unknown local __comparator : (§K#G§, §K#G§) -> int = <MTreeMap#C<K#G, V#G>>self.<reIU>comparator\(§K#G§, §K#G§) -> int\;
        ERROR: Unknown local var t : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <MTreeMap#C<K#G, V#G>>self.<emp>_root\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
        if(t == null) {
            this._root = new TreeMapEntry<K, V>(key, value, null);
            this._size = ERROR: Unknown 1.cast<uint>;
        } else {
            ERROR: Unknown local var cmp : int = 0;
            ERROR: Unknown local var parent : (^TreeMapEntry#C<K#G, V#G>)? = none<^TreeMapEntry#C<K#G, V#G>>;
            ERROR: Unknown do{
    (<lm>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ = <lm>t\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.cast<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)
    (<lm>cmp\int\ = <l>__comparator\(§K#G§, §K#G§) -> int\.<d>apply( = <l>key\§K#G§\,  = <lm>t\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUm>key\§K#G§\)\int\)
    if((<lm>cmp\int\ < 0)) (<lm>t\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>t\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
else if((<lm>cmp\int\ > 0)) (<lm>t\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>t\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
else {
    (<lm>t\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUm>value\§V#G§\ = <l>value\§V#G§\)
    return nil
}
} while((<lm>t\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>));
            ERROR: Unknown local e : TreeMapEntry#C<§K#G§, §V#G§> = <to>TreeMapEntry\TreeMapEntry#C.class\.<tcI>apply(key = <l>key\§K#G§\, value = <l>value\§V#G§\, parent = <lm>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)\TreeMapEntry#C<§K#G§, §V#G§>\;
            if(cmp < ERROR: Unknown 0) {
                if(parent == null) {
                    throw new RuntimeException("Not null");
                } else {
                    parent;
                }
                .left = e;
            } else {
                if(parent == null) {
                    throw new RuntimeException("Not null");
                } else {
                    parent;
                }
                .right = e;
            }
            fixAfterInsertionEntry(e);
            ERROR: Unknown <MTreeMap#C<K#G, V#G>>self.<emp>_size\uint\++;
        }
    }
    @Override
    public V removeForKey(K key) {
        ERROR: Unknown local _ : ^(^TreeMapEntry#C<§K#G§, §V#G§>)¿ = <MTreeMap#C<K#G, V#G>>self.<rdI>entryFor(key = <l>key\§K#G§\)\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
        if(_ != null) {
            return deleteEntry(_);
        } else {
            return null;
        }
    }
    private V deleteEntry(TreeMapEntry<K, V> entry) {
        ERROR: Unknown local var p : TreeMapEntry#C<§K#G§, §V#G§> = <l>entry\TreeMapEntry#C<§K#G§, §V#G§>\;
        ERROR: Unknown <MTreeMap#C<K#G, V#G>>self.<emp>_size\uint\--;
        if(p.left != null && p.right != null) {
            ERROR: Unknown local s : TreeMapEntry#C<§K#G§, §V#G§> = <lm>p\TreeMapEntry#C<§K#G§, §V#G§>\.<dI>next\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get;
            p.key = s.key;
            p.value = s.value;
            p = s;
        }
        ERROR: Unknown local replacement : (^TreeMapEntry#C<§K#G§, §V#G§>)? = if((<lm>p\TreeMapEntry#C<§K#G§, §V#G§>\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>)) <lm>p\TreeMapEntry#C<§K#G§, §V#G§>\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
else <lm>p\TreeMapEntry#C<§K#G§, §V#G§>\.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
        if(replacement != null) {
            replacement.parent = p.parent;
            if(p.parent == null) {
                this._root = ERROR: Unknown <l>replacement\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.cast<(^TreeMapEntry#C<§K#G§, §V#G§>)?>;
            } else {
                if(ERROR: Unknown {
    local __tmp_4_1 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>p\TreeMapEntry#C<§K#G§, §V#G§>\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    return ((<l>__tmp_4_1\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>) && (<l>__tmp_4_1\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == <lm>p\TreeMapEntry#C<§K#G§, §V#G§>\))
}) {
                    ERROR: Unknown {
    local __tmp_4_1 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>p\TreeMapEntry#C<§K#G§, §V#G§>\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_4_1\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == none<^TreeMapEntry#C<§K#G§, §V#G§>>)) throw "Not null"
else <l>__tmp_4_1\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
}.left = ERROR: Unknown <l>replacement\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.cast<(^TreeMapEntry#C<§K#G§, §V#G§>)?>;
                } else {
                    ERROR: Unknown {
    local __tmp_4_1 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>p\TreeMapEntry#C<§K#G§, §V#G§>\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_4_1\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == none<^TreeMapEntry#C<§K#G§, §V#G§>>)) throw "Not null"
else <l>__tmp_4_1\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
}.right = ERROR: Unknown <l>replacement\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.cast<(^TreeMapEntry#C<§K#G§, §V#G§>)?>;
                }
            }
            p.left = null;
            p.right = null;
            p.parent = null;
            if(p.color.equals(this.BLACK)) {
                fixAfterDeletionEntry(replacement);
            }
        } else {
            if(p.parent == null) {
                this._root = null;
            } else {
                if(p.color.equals(this.BLACK)) {
                    fixAfterDeletionEntry(p);
                }
                if(p.parent != null) {
                    if(ERROR: Unknown {
    local __tmp_4_1_0 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>p\TreeMapEntry#C<§K#G§, §V#G§>\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    return ((<l>__tmp_4_1_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>) && (<l>__tmp_4_1_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == <lm>p\TreeMapEntry#C<§K#G§, §V#G§>\))
}) {
                        ERROR: Unknown {
    local __tmp_4_1_0 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>p\TreeMapEntry#C<§K#G§, §V#G§>\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_4_1_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == none<^TreeMapEntry#C<§K#G§, §V#G§>>)) throw "Not null"
else <l>__tmp_4_1_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
}.left = null;
                    } else {
                        if(ERROR: Unknown {
    local __tmp_4_1_0 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>p\TreeMapEntry#C<§K#G§, §V#G§>\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    return ((<l>__tmp_4_1_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>) && (<l>__tmp_4_1_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == <lm>p\TreeMapEntry#C<§K#G§, §V#G§>\))
}) {
                            ERROR: Unknown {
    local __tmp_4_1_0 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>p\TreeMapEntry#C<§K#G§, §V#G§>\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_4_1_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == none<^TreeMapEntry#C<§K#G§, §V#G§>>)) throw "Not null"
else <l>__tmp_4_1_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
}.right = null;
                        }
                    }
                    p.parent = null;
                }
            }
        }
        return entry.value;
    }
    private void fixAfterInsertionEntry(TreeMapEntry<K, V> entry) {
        entry.color = this.RED;
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
        if(this._root != null) {
            this._root.color = this.BLACK;
        }
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
        if(x != null) {
            x.color = this.BLACK;
        }
    }
    private void rotateLeftP(TreeMapEntry<K, V> p) {
        if(p != null) {
            ERROR: Unknown local r : TreeMapEntry#C<§K#G§, §V#G§> = <l>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get;
            p.right = r.left;
            {
                ERROR: Unknown local __tmp_0_2 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <l>r\TreeMapEntry#C<§K#G§, §V#G§>\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
                if(__tmp_0_2 != null) {
                    __tmp_0_2.parent = ERROR: Unknown <l>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.cast<(^TreeMapEntry#C<§K#G§, §V#G§>)?>;
                }
            }
            r.parent = p.parent;
            if(p.parent == null) {
                this._root = r;
            } else {
                if(ERROR: Unknown {
    local __tmp_0_4 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <l>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    return ((<l>__tmp_0_4\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>) && (<l>__tmp_0_4\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == <l>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\))
}) {
                    ERROR: Unknown {
    local __tmp_0_4 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <l>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_0_4\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == none<^TreeMapEntry#C<§K#G§, §V#G§>>)) throw "Not null"
else <l>__tmp_0_4\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
}.left = r;
                } else {
                    ERROR: Unknown {
    local __tmp_0_4 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <l>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_0_4\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == none<^TreeMapEntry#C<§K#G§, §V#G§>>)) throw "Not null"
else <l>__tmp_0_4\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
}.right = r;
                }
            }
            r.left = ERROR: Unknown <l>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.cast<(^TreeMapEntry#C<§K#G§, §V#G§>)?>;
            p.parent = r;
        }
    }
    private void rotateRightP(TreeMapEntry<K, V> p) {
        if(p != null) {
            ERROR: Unknown local l : TreeMapEntry#C<§K#G§, §V#G§> = <l>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get;
            p.left = l.right;
            {
                ERROR: Unknown local __tmp_0_2 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <l>l\TreeMapEntry#C<§K#G§, §V#G§>\.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
                if(__tmp_0_2 != null) {
                    __tmp_0_2.parent = ERROR: Unknown <l>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.cast<(^TreeMapEntry#C<§K#G§, §V#G§>)?>;
                }
            }
            l.parent = p.parent;
            if(p.parent == null) {
                this._root = l;
            } else {
                if(ERROR: Unknown {
    local __tmp_0_4 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <l>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    return ((<l>__tmp_0_4\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>) && (<l>__tmp_0_4\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == <l>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\))
}) {
                    ERROR: Unknown {
    local __tmp_0_4 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <l>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_0_4\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == none<^TreeMapEntry#C<§K#G§, §V#G§>>)) throw "Not null"
else <l>__tmp_0_4\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
}.right = l;
                } else {
                    ERROR: Unknown {
    local __tmp_0_4 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <l>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_0_4\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == none<^TreeMapEntry#C<§K#G§, §V#G§>>)) throw "Not null"
else <l>__tmp_0_4\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
}.left = l;
                }
            }
            l.right = ERROR: Unknown <l>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.cast<(^TreeMapEntry#C<§K#G§, §V#G§>)?>;
            p.parent = l;
        }
    }
    public Tuple2<K, V> pollFirst() {
        ERROR: Unknown local entry : ^(^TreeMapEntry#C<§K#G§, §V#G§>)¿ = <MTreeMap#C<K#G, V#G>>self.<rdI>firstEntry\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
        if(entry != null) {
            deleteEntry(entry);
            return ERROR: Unknown (<l>entry\^(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUm>key\§K#G§\, <l>entry\^(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUm>value\§V#G§\);
        } else {
            return null;
        }
    }
    public MTreeMap(F2<K, K, Integer> comparator) {
    }
    public V objectForKeyOrUpdateWith(K key,F<Void, V> orUpdateWith) {
        ERROR: Unknown local __tmp : §(V#G)?§ = <MMap#T<K#G, V#G>>self.<rdIa>opt(key = <l>key\§K#G§\)\(§V#G§)?\;
        if(__tmp != null) {
            return __tmp;
        } else {
            ERROR: Unknown local init : V#G = <l>orUpdateWith\void -> §V#G§\();
            setKeyValue(key, init);
            return init;
        }
    }
    public V modifyKeyBy(K key,F<V, V> by) {
        ERROR: Unknown local newObject : (§V#G§)? = <l>by\(§V#G§)? -> (§V#G§)?\.<d>apply( = <MMap#T<K#G, V#G>>self.<rdIa>opt(key = <l>key\§K#G§\)\(§V#G§)?\)\(§V#G§)?\;
        if(newObject == null) {
            removeForKey(key);
        } else {
            setKeyValue(key, newObject);
        }
        return newObject;
    }
    public V takeKey(K key) {
        ERROR: Unknown local ret : (§V#G§)? = <MMap#T<K#G, V#G>>self.<rdIa>opt(key = <l>key\§K#G§\)\(§V#G§)?\;
        removeForKey(key);
        return ret;
    }
    @Override
    public void appendItem(Tuple2<K, V> item) {
        setKeyValue(item.b, item.a);
    }
    @Override
    public boolean removeItem(Tuple2<K, V> item) {
        return removeForKey(item.a) != null;
    }
    @Override
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