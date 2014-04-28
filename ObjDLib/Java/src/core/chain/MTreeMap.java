package core.chain;

public class MTreeMap<K, V> extends TreeMap<K, V> implements MMap<K, V> {
    private TreeMapEntry<K, V> _root;
    private int _size;
    public final MTreeMapKeySet<K> keys;
    @Override
    public MTreeMapKeySet<K> keys() {
        return keys;
    }
    public static <K extends Comparable<K>, V> MTreeMap<K, V> apply() {
        return new MTreeMap<K, V>(new F2<K, K, Integer>() {
            @Override
            public Integer apply(final K a, final K b) {
                return a.compareTo(b);
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
    public void assignImMap(final ImMap<K, V> imMap) {
        if(imMap instanceof ImTreeMap) {
            final ImTreeMap<K, V> m = ((ImTreeMap<K, V>)imMap);
            this._root = ERROR: Unknown <l>m\ImTreeMap#C<§K#G§, §V#G§>\.<eIUo>root\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<dI>copy(parent = none<^TreeMapEntry#C<§K#G§, §V#G§>>)\TreeMapEntry#C<§K#G§, §V#G§>\;
            this._size = m.count;
        } else {
            this.clear();
            {
                final Iterator<Tuple<K, V>> __inline__0_1_i = imMap.iterator();
                while(__inline__0_1_i.hasNext()) {
                    final Tuple<K, V> _ = __inline__0_1_i.next();
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
    public MIterator<Tuple<K, V>> mutableIterator() {
        return MTreeMapIterator.<K, V>applyMapEntry(this, this.firstEntry());
    }
    @Override
    public void setKeyValue(final K key, final V value) {
        final F2<K, K, Integer> __comparator = this.comparator;
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
            final TreeMapEntry<K, V> e = new TreeMapEntry<K, V>(key, value, parent);
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
            this._size++;
        }
    }
    @Override
    public V removeForKey(final K key) {
        final TreeMapEntry<K, V> _ = entryForKey(key);
        if(_ != null) {
            return deleteEntry(_);
        } else {
            return null;
        }
    }
    private V deleteEntry(final TreeMapEntry<K, V> entry) {
        TreeMapEntry<K, V> p = entry;
        this._size--;
        if(p.left != null && p.right != null) {
            final TreeMapEntry<K, V> __tmp_2_0 = p.next();
            if(__tmp_2_0 == null) {
                throw new RuntimeException("Not null");
            } else {
                final TreeMapEntry<K, V> s = __tmp_2_0;
            }
            p.key = s.key;
            p.value = s.value;
            p = s;
        }
        final TreeMapEntry<K, V> replacement = (p.left != null) ? (p.left) : (p.right);
        if(replacement != null) {
            replacement.parent = p.parent;
            if(p.parent == null) {
                this._root = ((TreeMapEntry<K, V>)replacement);
            } else {
                final TreeMapEntry<K, V> __tmp_4_1 = p.parent;
                if(__tmp_4_1 == null) {
                    throw new RuntimeException("Not null");
                } else {
                    __tmp_4_1;
                }
                final TreeMapEntry<K, V> __tmp_4_1 = .left;
                if(__tmp_4_1 != null && __tmp_4_1.equals(p)) {
                    final TreeMapEntry<K, V> __tmp_4_1 = p.parent;
                    if(__tmp_4_1 == null) {
                        throw new RuntimeException("Not null");
                    } else {
                        __tmp_4_1;
                    }
                    .left = ((TreeMapEntry<K, V>)replacement);
                } else {
                    final TreeMapEntry<K, V> __tmp_4_1 = p.parent;
                    if(__tmp_4_1 == null) {
                        throw new RuntimeException("Not null");
                    } else {
                        __tmp_4_1;
                    }
                    .right = ((TreeMapEntry<K, V>)replacement);
                }
            }
            p.left = null;
            p.right = null;
            p.parent = null;
            if(p.color == this.BLACK) {
                fixAfterDeletionEntry(replacement);
            }
        } else {
            if(p.parent == null) {
                this._root = null;
            } else {
                if(p.color == this.BLACK) {
                    fixAfterDeletionEntry(p);
                }
                if(p.parent != null) {
                    final TreeMapEntry<K, V> __tmp_4_1_0 = p.parent;
                    if(__tmp_4_1_0 == null) {
                        throw new RuntimeException("Not null");
                    } else {
                        __tmp_4_1_0;
                    }
                    final TreeMapEntry<K, V> __tmp_4_1_0 = .left;
                    if(__tmp_4_1_0 != null && __tmp_4_1_0.equals(p)) {
                        final TreeMapEntry<K, V> __tmp_4_1_0 = p.parent;
                        if(__tmp_4_1_0 == null) {
                            throw new RuntimeException("Not null");
                        } else {
                            __tmp_4_1_0;
                        }
                        .left = null;
                    } else {
                        final TreeMapEntry<K, V> __tmp_4_1_0 = p.parent;
                        if(__tmp_4_1_0 == null) {
                            throw new RuntimeException("Not null");
                        } else {
                            __tmp_4_1_0;
                        }
                        final TreeMapEntry<K, V> __tmp_4_1_0 = .right;
                        if(__tmp_4_1_0 != null && __tmp_4_1_0.equals(p)) {
                            final TreeMapEntry<K, V> __tmp_4_1_0 = p.parent;
                            if(__tmp_4_1_0 == null) {
                                throw new RuntimeException("Not null");
                            } else {
                                __tmp_4_1_0;
                            }
                            .right = null;
                        }
                    }
                    p.parent = null;
                }
            }
        }
        return entry.value;
    }
    private void fixAfterInsertionEntry(final TreeMapEntry<K, V> entry) {
        entry.color = this.RED;
        TreeMapEntry<K, V> x = entry;
        final TreeMapEntry<K, V> __tmp_2 = x.parent;
        if(__tmp_2 == null) {
            throw new RuntimeException("Not null");
        } else {
            __tmp_2;
        }
        while(x != null && (this._root == null || !(this._root.equals(x))) && .color == this.RED) {
            final TreeMapEntry<K, V> __tmp_2_0_l = x.parent;
            final TreeMapEntry<K, V> __tmp_2_0_r = ERROR: Unknown <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
            if(__tmp_2_0_l == __tmp_2_0_r || (__tmp_2_0_l != null && __tmp_2_0_r != null && __tmp_2_0_l.equals(__tmp_2_0_r))) {
                final TreeMapEntry<K, V> y = ERROR: Unknown <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
                if(y != null && y.color == this.RED) {
                    {
                        final TreeMapEntry<K, V> __tmp_2_0_1_0 = x.parent;
                        if(__tmp_2_0_1_0 != null) {
                            __tmp_2_0_1_0.color = this.BLACK;
                        }
                    }
                    y.color = this.BLACK;
                    {
                        final TreeMapEntry<K, V> __tmp_2_0_1_2 = ERROR: Unknown <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
                        if(__tmp_2_0_1_2 != null) {
                            __tmp_2_0_1_2.color = this.RED;
                        }
                    }
                    x = ERROR: Unknown <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
                } else {
                    final TreeMapEntry<K, V> __tmp_2_0_1_0 = ERROR: Unknown <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
                    if(__tmp_2_0_1_0 != null && __tmp_2_0_1_0.equals(x)) {
                        x = x.parent;
                        rotateLeftP(x);
                    }
                    {
                        final TreeMapEntry<K, V> __tmp_2_0_1_1 = x.parent;
                        if(__tmp_2_0_1_1 != null) {
                            __tmp_2_0_1_1.color = this.BLACK;
                        }
                    }
                    {
                        final TreeMapEntry<K, V> __tmp_2_0_1_2 = ERROR: Unknown <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
                        if(__tmp_2_0_1_2 != null) {
                            __tmp_2_0_1_2.color = this.RED;
                        }
                    }
                    rotateRightP(ERROR: Unknown <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\);
                }
            } else {
                final TreeMapEntry<K, V> y = ERROR: Unknown <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
                if(y != null && y.color == this.RED) {
                    {
                        final TreeMapEntry<K, V> __tmp_2_0_1_0 = x.parent;
                        if(__tmp_2_0_1_0 != null) {
                            __tmp_2_0_1_0.color = this.BLACK;
                        }
                    }
                    y.color = this.BLACK;
                    {
                        final TreeMapEntry<K, V> __tmp_2_0_1_2 = ERROR: Unknown <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
                        if(__tmp_2_0_1_2 != null) {
                            __tmp_2_0_1_2.color = this.RED;
                        }
                    }
                    x = ERROR: Unknown <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
                } else {
                    final TreeMapEntry<K, V> __tmp_2_0_1_0 = ERROR: Unknown <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
                    if(__tmp_2_0_1_0 != null && __tmp_2_0_1_0.equals(x)) {
                        x = x.parent;
                        rotateRightP(x);
                    }
                    {
                        final TreeMapEntry<K, V> __tmp_2_0_1_1 = x.parent;
                        if(__tmp_2_0_1_1 != null) {
                            __tmp_2_0_1_1.color = this.BLACK;
                        }
                    }
                    {
                        final TreeMapEntry<K, V> __tmp_2_0_1_2 = ERROR: Unknown <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
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
    private void fixAfterDeletionEntry(final TreeMapEntry<K, V> entry) {
        TreeMapEntry<K, V> x = entry;
        while(x != null && (this._root == null || !(this._root.equals(x))) && x.color == this.BLACK) {
            final TreeMapEntry<K, V> __tmp_1_0 = ERROR: Unknown <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
            if(__tmp_1_0 != null && __tmp_1_0.equals(x)) {
                TreeMapEntry<K, V> sib = ERROR: Unknown <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
                if(sib != null && sib.color == this.RED) {
                    sib.color = this.BLACK;
                    {
                        final TreeMapEntry<K, V> __tmp_1_0_1_1 = x.parent;
                        if(__tmp_1_0_1_1 != null) {
                            __tmp_1_0_1_1.color = this.RED;
                        }
                    }
                    rotateLeftP(x.parent);
                    sib = ERROR: Unknown <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
                }
                final TreeMapEntry<K, V> __tmp_1_0_2 = ERROR: Unknown <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
                final TreeMapEntry<K, V> __tmp_1_0_2 = ERROR: Unknown <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
                if((__tmp_1_0_2 != null) ? (ERROR: Unknown <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.color) : (this.BLACK) == this.BLACK && (__tmp_1_0_2 != null) ? (ERROR: Unknown <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.color) : (this.BLACK) == this.BLACK) {
                    if(sib != null) {
                        sib.color = this.RED;
                    }
                    x = x.parent;
                } else {
                    final TreeMapEntry<K, V> __tmp_1_0_2_0 = ERROR: Unknown <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
                    if((__tmp_1_0_2_0 != null) ? (ERROR: Unknown <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.color) : (this.BLACK) == this.BLACK) {
                        {
                            final TreeMapEntry<K, V> __tmp_1_0_2_0_0 = ERROR: Unknown <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
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
                        final TreeMapEntry<K, V> __tmp_1_0_2_1 = x.parent;
                        sib.color = (__tmp_1_0_2_1 != null) ? (x.parent.color) : (this.BLACK);
                    }
                    {
                        final TreeMapEntry<K, V> __tmp_1_0_2_2 = x.parent;
                        if(__tmp_1_0_2_2 != null) {
                            __tmp_1_0_2_2.color = this.BLACK;
                        }
                    }
                    {
                        final TreeMapEntry<K, V> __tmp_1_0_2_3 = ERROR: Unknown <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
                        if(__tmp_1_0_2_3 != null) {
                            __tmp_1_0_2_3.color = this.BLACK;
                        }
                    }
                    rotateLeftP(x.parent);
                    x = this._root;
                }
            } else {
                TreeMapEntry<K, V> sib = ERROR: Unknown <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
                if(sib != null && sib.color == this.RED) {
                    sib.color = this.BLACK;
                    {
                        final TreeMapEntry<K, V> __tmp_1_0_1_1 = x.parent;
                        if(__tmp_1_0_1_1 != null) {
                            __tmp_1_0_1_1.color = this.RED;
                        }
                    }
                    rotateRightP(x.parent);
                    sib = ERROR: Unknown <lm>x\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
                }
                final TreeMapEntry<K, V> __tmp_1_0_2 = ERROR: Unknown <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
                final TreeMapEntry<K, V> __tmp_1_0_2 = ERROR: Unknown <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
                if((__tmp_1_0_2 != null) ? (ERROR: Unknown <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.color) : (this.BLACK) == this.BLACK && (__tmp_1_0_2 != null) ? (ERROR: Unknown <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.color) : (this.BLACK) == this.BLACK) {
                    if(sib != null) {
                        sib.color = this.RED;
                    }
                    x = x.parent;
                } else {
                    final TreeMapEntry<K, V> __tmp_1_0_2_0 = ERROR: Unknown <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
                    if((__tmp_1_0_2_0 != null) ? (ERROR: Unknown <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.color) : (this.BLACK) == this.BLACK) {
                        {
                            final TreeMapEntry<K, V> __tmp_1_0_2_0_0 = ERROR: Unknown <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
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
                        final TreeMapEntry<K, V> __tmp_1_0_2_1 = x.parent;
                        sib.color = (__tmp_1_0_2_1 != null) ? (x.parent.color) : (this.BLACK);
                    }
                    {
                        final TreeMapEntry<K, V> __tmp_1_0_2_2 = x.parent;
                        if(__tmp_1_0_2_2 != null) {
                            __tmp_1_0_2_2.color = this.BLACK;
                        }
                    }
                    {
                        final TreeMapEntry<K, V> __tmp_1_0_2_3 = ERROR: Unknown <lm>sib\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
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
    private void rotateLeftP(final TreeMapEntry<K, V> p) {
        if(p != null) {
            final TreeMapEntry<K, V> __tmp_0_0 = p.right;
            if(__tmp_0_0 == null) {
                throw new RuntimeException("Not null");
            } else {
                final TreeMapEntry<K, V> r = __tmp_0_0;
            }
            p.right = r.left;
            {
                final TreeMapEntry<K, V> __tmp_0_2 = r.left;
                if(__tmp_0_2 != null) {
                    __tmp_0_2.parent = ((TreeMapEntry<K, V>)p);
                }
            }
            r.parent = p.parent;
            if(p.parent == null) {
                this._root = r;
            } else {
                final TreeMapEntry<K, V> __tmp_0_4 = p.parent;
                if(__tmp_0_4 == null) {
                    throw new RuntimeException("Not null");
                } else {
                    __tmp_0_4;
                }
                final TreeMapEntry<K, V> __tmp_0_4 = .left;
                if(__tmp_0_4 != null && __tmp_0_4.equals(p)) {
                    final TreeMapEntry<K, V> __tmp_0_4 = p.parent;
                    if(__tmp_0_4 == null) {
                        throw new RuntimeException("Not null");
                    } else {
                        __tmp_0_4;
                    }
                    .left = r;
                } else {
                    final TreeMapEntry<K, V> __tmp_0_4 = p.parent;
                    if(__tmp_0_4 == null) {
                        throw new RuntimeException("Not null");
                    } else {
                        __tmp_0_4;
                    }
                    .right = r;
                }
            }
            r.left = ((TreeMapEntry<K, V>)p);
            p.parent = r;
        }
    }
    private void rotateRightP(final TreeMapEntry<K, V> p) {
        if(p != null) {
            final TreeMapEntry<K, V> __tmp_0_0 = p.left;
            if(__tmp_0_0 == null) {
                throw new RuntimeException("Not null");
            } else {
                final TreeMapEntry<K, V> l = __tmp_0_0;
            }
            p.left = l.right;
            {
                final TreeMapEntry<K, V> __tmp_0_2 = l.right;
                if(__tmp_0_2 != null) {
                    __tmp_0_2.parent = ((TreeMapEntry<K, V>)p);
                }
            }
            l.parent = p.parent;
            if(p.parent == null) {
                this._root = l;
            } else {
                final TreeMapEntry<K, V> __tmp_0_4 = p.parent;
                if(__tmp_0_4 == null) {
                    throw new RuntimeException("Not null");
                } else {
                    __tmp_0_4;
                }
                final TreeMapEntry<K, V> __tmp_0_4 = .right;
                if(__tmp_0_4 != null && __tmp_0_4.equals(p)) {
                    final TreeMapEntry<K, V> __tmp_0_4 = p.parent;
                    if(__tmp_0_4 == null) {
                        throw new RuntimeException("Not null");
                    } else {
                        __tmp_0_4;
                    }
                    .right = l;
                } else {
                    final TreeMapEntry<K, V> __tmp_0_4 = p.parent;
                    if(__tmp_0_4 == null) {
                        throw new RuntimeException("Not null");
                    } else {
                        __tmp_0_4;
                    }
                    .left = l;
                }
            }
            l.right = ((TreeMapEntry<K, V>)p);
            p.parent = l;
        }
    }
    public Tuple<K, V> pollFirst() {
        final TreeMapEntry<K, V> entry = this.firstEntry();
        if(entry != null) {
            deleteEntry(entry);
            return new Tuple<K, V>(entry.key, entry.value);
        } else {
            return null;
        }
    }
    public MTreeMap(final F2<K, K, Integer> comparator) {
        super(comparator);
        this._root = null;
        this._size = ((int)0);
        this.keys = new MTreeMapKeySet<K>(this);
    }
    public V objectForKeyOrUpdateWith(final K key, final F0<V> orUpdateWith) {
        final V __tmp = optKey(key);
        if(__tmp != null) {
            return __tmp;
        } else {
            final V init = orUpdateWith.apply();
            setKeyValue(key, init);
            return init;
        }
    }
    public V modifyKeyBy(final K key, final F<V, V> by) {
        final V newObject = by.apply(optKey(key));
        if(newObject == null) {
            removeForKey(key);
        } else {
            setKeyValue(key, newObject);
        }
        return newObject;
    }
    public V takeKey(final K key) {
        final V ret = optKey(key);
        removeForKey(key);
        return ret;
    }
    @Override
    public void appendItem(final Tuple<K, V> item) {
        setKeyValue(item.b, item.a);
    }
    @Override
    public boolean removeItem(final Tuple<K, V> item) {
        return removeForKey(item.a) != null;
    }
    @Override
    public boolean removeItem(final T item) {
        final MIterator<T> i = this.mutableIterator();
        boolean ret = false;
        while(i.hasNext()) {
            if(i.next().equals(item)) {
                i.remove();
                ret = true;
            }
        }
        return ret;
    }
    public void mutableFilterBy(final F<T, Boolean> by) {
        final MIterator<T> i = this.mutableIterator();
        while(i.hasNext()) {
            if(by.apply(i.next())) {
                i.remove();
            }
        }
    }
}