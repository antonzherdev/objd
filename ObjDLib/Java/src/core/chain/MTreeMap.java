package core.chain;

public class MTreeMap<K, V> extends TreeMap<K, V> implements MMap<K, V> {
    private TreeMapEntry<K, V> _root = null;
    private int _size = ((int)0);
    public final MTreeMapKeySet<K> keys = new MTreeMapKeySet<K>(this);
    public static  <K extends Comparable<K>, V> MTreeMap<K, V> apply() {
        return new MTreeMap<K, V>(new F2<K, K, Integer>() {
            @Override
            public Integer apply(K a,K b) {
                ERROR: Unknown weak return <l>a\K#G\.<rdIb>compare(to = <l>b\K#G\)\int\;
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
        if(imMap instanceof ImTreeMap) {
            ImTreeMap<K, V> m = ((ImTreeMap<K, V>)imMap);
            this._root = ERROR: Unknown <l>m\ImTreeMap#C<§K#G§, §V#G§>\.<eIUo>root\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<dI>copy(parent = none<^TreeMapEntry#C<§K#G§, §V#G§>>)\TreeMapEntry#C<§K#G§, §V#G§>\;
            this._size = m.count;
        } else {
            this.clear();
            {
                Iterator<Tuple2<K, V>> __inline__0_1_i = imMap.iterator();
                while(__inline__0_1_i.hasNext()) {
                    Tuple2<K, V> _ = __inline__0_1_i.next();
                    appendItem(_);
                }
            }
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
        this._size = ((int)0);
        this._root = null;
    }
    @Override
    public MIterator<Tuple2<K, V>> mutableIterator() {
        return MTreeMapIterator.<K, V>applyMapEntry(this, this.firstEntry());
    }
    @Override
    public void setKeyValue(K key,V value) {
        F2<K, K, Integer> __comparator = this.comparator;
        TreeMapEntry<K, V> t = this._root;
        if(t == null) {
            this._root = new TreeMapEntry<K, V>(key, value, null);
            this._size = ((int)1);
        } else {
            int cmp = 0;
            TreeMapEntry<K, V> parent = null;
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
            TreeMapEntry<K, V> e = new TreeMapEntry<K, V>(key, value, parent);
            if(cmp < 0) {
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
        TreeMapEntry<K, V> _ = entryForKey(key);
        if(_ != null) {
            return deleteEntry(_);
        } else {
            return null;
        }
    }
    private V deleteEntry(TreeMapEntry<K, V> entry) {
        TreeMapEntry<K, V> p = entry;
        ERROR: Unknown <MTreeMap#C<K#G, V#G>>self.<emp>_size\uint\--;
        if(p.left != null && p.right != null) {
            TreeMapEntry<K, V> s = ERROR: Unknown {
    local __tmp_2_0 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>p\TreeMapEntry#C<§K#G§, §V#G§>\.<dI>next\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_2_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == none<^TreeMapEntry#C<§K#G§, §V#G§>>)) throw "Not null"
else <l>__tmp_2_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
};
            p.key = s.key;
            p.value = s.value;
            p = s;
        }
        TreeMapEntry<K, V> replacement = (p.left != null) ? (p.left) : (p.right);
        if(replacement != null) {
            replacement.parent = p.parent;
            if(p.parent == null) {
                this._root = ((TreeMapEntry<K, V>)replacement);
            } else {
                if(ERROR: Unknown {
    local __tmp_4_1 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>p\TreeMapEntry#C<§K#G§, §V#G§>\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    return ((<l>__tmp_4_1\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>) && (<l>__tmp_4_1\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == <lm>p\TreeMapEntry#C<§K#G§, §V#G§>\))
}) {
                    ERROR: Unknown {
    local __tmp_4_1 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>p\TreeMapEntry#C<§K#G§, §V#G§>\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_4_1\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == none<^TreeMapEntry#C<§K#G§, §V#G§>>)) throw "Not null"
else <l>__tmp_4_1\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
}.left = ((TreeMapEntry<K, V>)replacement);
                } else {
                    ERROR: Unknown {
    local __tmp_4_1 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>p\TreeMapEntry#C<§K#G§, §V#G§>\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_4_1\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == none<^TreeMapEntry#C<§K#G§, §V#G§>>)) throw "Not null"
else <l>__tmp_4_1\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
}.right = ((TreeMapEntry<K, V>)replacement);
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
        TreeMapEntry<K, V> x = entry;
        while(x != null && (this._root == null || this._root.equals(x)) && ERROR: Unknown {
    local __tmp_2 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == none<^TreeMapEntry#C<§K#G§, §V#G§>>)) throw "Not null"
else <l>__tmp_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
}.color.equals(this.RED)) {
            if(ERROR: Unknown {
    local __tmp_2_0_l : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    local __tmp_2_0_r : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    return ((<l>__tmp_2_0_l\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ === <l>__tmp_2_0_r\(^TreeMapEntry#C<§K#G§, §V#G§>)?\) || (((<l>__tmp_2_0_l\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>) && (<l>__tmp_2_0_r\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>)) && (<l>__tmp_2_0_l\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == <l>__tmp_2_0_r\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)))
}) {
                TreeMapEntry<K, V> y = ERROR: Unknown <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
                if(y != null && y.color.equals(this.RED)) {
                    {
                        TreeMapEntry<K, V> __tmp_2_0_1_0 = x.parent;
                        if(__tmp_2_0_1_0 != null) {
                            __tmp_2_0_1_0.color = this.BLACK;
                        }
                    }
                    y.color = this.BLACK;
                    {
                        TreeMapEntry<K, V> __tmp_2_0_1_2 = ERROR: Unknown <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
                        if(__tmp_2_0_1_2 != null) {
                            __tmp_2_0_1_2.color = this.RED;
                        }
                    }
                    x = ERROR: Unknown <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
                } else {
                    if(ERROR: Unknown {
    local __tmp_2_0_1_0 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    return ((<l>__tmp_2_0_1_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>) && (<l>__tmp_2_0_1_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\))
}) {
                        x = x.parent;
                        rotateLeftP(x);
                    }
                    {
                        TreeMapEntry<K, V> __tmp_2_0_1_1 = x.parent;
                        if(__tmp_2_0_1_1 != null) {
                            __tmp_2_0_1_1.color = this.BLACK;
                        }
                    }
                    {
                        TreeMapEntry<K, V> __tmp_2_0_1_2 = ERROR: Unknown <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
                        if(__tmp_2_0_1_2 != null) {
                            __tmp_2_0_1_2.color = this.RED;
                        }
                    }
                    rotateRightP(ERROR: Unknown <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\);
                }
            } else {
                TreeMapEntry<K, V> y = ERROR: Unknown <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
                if(y != null && y.color.equals(this.RED)) {
                    {
                        TreeMapEntry<K, V> __tmp_2_0_1_0 = x.parent;
                        if(__tmp_2_0_1_0 != null) {
                            __tmp_2_0_1_0.color = this.BLACK;
                        }
                    }
                    y.color = this.BLACK;
                    {
                        TreeMapEntry<K, V> __tmp_2_0_1_2 = ERROR: Unknown <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
                        if(__tmp_2_0_1_2 != null) {
                            __tmp_2_0_1_2.color = this.RED;
                        }
                    }
                    x = ERROR: Unknown <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
                } else {
                    if(ERROR: Unknown {
    local __tmp_2_0_1_0 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    return ((<l>__tmp_2_0_1_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>) && (<l>__tmp_2_0_1_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\))
}) {
                        x = x.parent;
                        rotateRightP(x);
                    }
                    {
                        TreeMapEntry<K, V> __tmp_2_0_1_1 = x.parent;
                        if(__tmp_2_0_1_1 != null) {
                            __tmp_2_0_1_1.color = this.BLACK;
                        }
                    }
                    {
                        TreeMapEntry<K, V> __tmp_2_0_1_2 = ERROR: Unknown <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
                        if(__tmp_2_0_1_2 != null) {
                            __tmp_2_0_1_2.color = this.RED;
                        }
                    }
                    rotateLeftP(ERROR: Unknown <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\);
                }
            }
        }
        if(this._root != null) {
            this._root.color = this.BLACK;
        }
    }
    private void fixAfterDeletionEntry(TreeMapEntry<K, V> entry) {
        TreeMapEntry<K, V> x = entry;
        while(x != null && (this._root == null || this._root.equals(x)) && x.color.equals(this.BLACK)) {
            if(ERROR: Unknown {
    local __tmp_1_0 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    return ((<l>__tmp_1_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>) && (<l>__tmp_1_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\))
}) {
                TreeMapEntry<K, V> sib = ERROR: Unknown <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
                if(sib != null && sib.color.equals(this.RED)) {
                    sib.color = this.BLACK;
                    {
                        TreeMapEntry<K, V> __tmp_1_0_1_1 = x.parent;
                        if(__tmp_1_0_1_1 != null) {
                            __tmp_1_0_1_1.color = this.RED;
                        }
                    }
                    rotateLeftP(x.parent);
                    sib = ERROR: Unknown <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
                }
                if(ERROR: Unknown {
    local __tmp_1_0_2 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_1_0_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>color\int\
else <MTreeMap#C<K#G, V#G>>self.<reIt>BLACK\int\
}.equals(this.BLACK) && ERROR: Unknown {
    local __tmp_1_0_2 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_1_0_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>color\int\
else <MTreeMap#C<K#G, V#G>>self.<reIt>BLACK\int\
}.equals(this.BLACK)) {
                    if(sib != null) {
                        sib.color = this.RED;
                    }
                    x = x.parent;
                } else {
                    if(ERROR: Unknown {
    local __tmp_1_0_2_0 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_1_0_2_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>color\int\
else <MTreeMap#C<K#G, V#G>>self.<reIt>BLACK\int\
}.equals(this.BLACK)) {
                        {
                            TreeMapEntry<K, V> __tmp_1_0_2_0_0 = ERROR: Unknown <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
                            if(__tmp_1_0_2_0_0 != null) {
                                __tmp_1_0_2_0_0.color = this.BLACK;
                            }
                        }
                        if(sib != null) {
                            sib.color = this.RED;
                        }
                        rotateRightP(sib);
                        sib = ERROR: Unknown <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
                    }
                    if(sib != null) {
                        sib.color = ERROR: Unknown {
    local __tmp_1_0_2_1 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_1_0_2_1\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) return <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>color\int\
else return <MTreeMap#C<K#G, V#G>>self.<reIt>BLACK\int\
};
                    }
                    {
                        TreeMapEntry<K, V> __tmp_1_0_2_2 = x.parent;
                        if(__tmp_1_0_2_2 != null) {
                            __tmp_1_0_2_2.color = this.BLACK;
                        }
                    }
                    {
                        TreeMapEntry<K, V> __tmp_1_0_2_3 = ERROR: Unknown <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
                        if(__tmp_1_0_2_3 != null) {
                            __tmp_1_0_2_3.color = this.BLACK;
                        }
                    }
                    rotateLeftP(x.parent);
                    x = this._root;
                }
            } else {
                TreeMapEntry<K, V> sib = ERROR: Unknown <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
                if(sib != null && sib.color.equals(this.RED)) {
                    sib.color = this.BLACK;
                    {
                        TreeMapEntry<K, V> __tmp_1_0_1_1 = x.parent;
                        if(__tmp_1_0_1_1 != null) {
                            __tmp_1_0_1_1.color = this.RED;
                        }
                    }
                    rotateRightP(x.parent);
                    sib = ERROR: Unknown <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
                }
                if(ERROR: Unknown {
    local __tmp_1_0_2 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_1_0_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>color\int\
else <MTreeMap#C<K#G, V#G>>self.<reIt>BLACK\int\
}.equals(this.BLACK) && ERROR: Unknown {
    local __tmp_1_0_2 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_1_0_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>color\int\
else <MTreeMap#C<K#G, V#G>>self.<reIt>BLACK\int\
}.equals(this.BLACK)) {
                    if(sib != null) {
                        sib.color = this.RED;
                    }
                    x = x.parent;
                } else {
                    if(ERROR: Unknown {
    local __tmp_1_0_2_0 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_1_0_2_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>color\int\
else <MTreeMap#C<K#G, V#G>>self.<reIt>BLACK\int\
}.equals(this.BLACK)) {
                        {
                            TreeMapEntry<K, V> __tmp_1_0_2_0_0 = ERROR: Unknown <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
                            if(__tmp_1_0_2_0_0 != null) {
                                __tmp_1_0_2_0_0.color = this.BLACK;
                            }
                        }
                        if(sib != null) {
                            sib.color = this.RED;
                        }
                        rotateLeftP(sib);
                        sib = ERROR: Unknown <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
                    }
                    if(sib != null) {
                        sib.color = ERROR: Unknown {
    local __tmp_1_0_2_1 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_1_0_2_1\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<(^TreeMapEntry#C<§K#G§, §V#G§>)?>)) return <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIm>color\int\
else return <MTreeMap#C<K#G, V#G>>self.<reIt>BLACK\int\
};
                    }
                    {
                        TreeMapEntry<K, V> __tmp_1_0_2_2 = x.parent;
                        if(__tmp_1_0_2_2 != null) {
                            __tmp_1_0_2_2.color = this.BLACK;
                        }
                    }
                    {
                        TreeMapEntry<K, V> __tmp_1_0_2_3 = ERROR: Unknown <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
                        if(__tmp_1_0_2_3 != null) {
                            __tmp_1_0_2_3.color = this.BLACK;
                        }
                    }
                    rotateRightP(x.parent);
                    x = this._root;
                }
            }
        }
        if(x != null) {
            x.color = this.BLACK;
        }
    }
    private void rotateLeftP(TreeMapEntry<K, V> p) {
        if(p != null) {
            TreeMapEntry<K, V> r = ERROR: Unknown {
    local __tmp_0_0 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <l>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_0_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == none<^TreeMapEntry#C<§K#G§, §V#G§>>)) throw "Not null"
else <l>__tmp_0_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
};
            p.right = r.left;
            {
                TreeMapEntry<K, V> __tmp_0_2 = r.left;
                if(__tmp_0_2 != null) {
                    __tmp_0_2.parent = ((TreeMapEntry<K, V>)p);
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
            r.left = ((TreeMapEntry<K, V>)p);
            p.parent = r;
        }
    }
    private void rotateRightP(TreeMapEntry<K, V> p) {
        if(p != null) {
            TreeMapEntry<K, V> l = ERROR: Unknown {
    local __tmp_0_0 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <l>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp_0_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == none<^TreeMapEntry#C<§K#G§, §V#G§>>)) throw "Not null"
else <l>__tmp_0_0\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
};
            p.left = l.right;
            {
                TreeMapEntry<K, V> __tmp_0_2 = l.right;
                if(__tmp_0_2 != null) {
                    __tmp_0_2.parent = ((TreeMapEntry<K, V>)p);
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
            l.right = ((TreeMapEntry<K, V>)p);
            p.parent = l;
        }
    }
    public Tuple2<K, V> pollFirst() {
        TreeMapEntry<K, V> entry = this.firstEntry();
        if(entry != null) {
            deleteEntry(entry);
            return ERROR: Unknown (<l>entry\^(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUm>key\§K#G§\, <l>entry\^(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUm>value\§V#G§\);
        } else {
            return null;
        }
    }
    public MTreeMap(F2<K, K, Integer> comparator) {
        super(comparator);
    }
    public V objectForKeyOrUpdateWith(K key,F<Void, V> orUpdateWith) {
        V __tmp = optKey(key);
        if(__tmp != null) {
            return __tmp;
        } else {
            V init = ERROR: Unknown <l>orUpdateWith\void -> §V#G§\();
            setKeyValue(key, init);
            return init;
        }
    }
    public V modifyKeyBy(K key,F<V, V> by) {
        V newObject = by.apply(optKey(key));
        if(newObject == null) {
            removeForKey(key);
        } else {
            setKeyValue(key, newObject);
        }
        return newObject;
    }
    public V takeKey(K key) {
        V ret = optKey(key);
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
        MIterator<T> i = this.mutableIterator();
        boolean ret = false;
        while(i.hasNext()) {
            if(i.next().equals(item)) {
                i.remove();
                ret = true;
            }
        }
        return ret;
    }
    public void mutableFilterBy(F<T, Boolean> by) {
        MIterator<T> i = this.mutableIterator();
        while(i.hasNext()) {
            if(by.apply(i.next())) {
                i.remove();
            }
        }
    }
}