package objd.collection;

import objd.lang.*;

public class MTreeMap<K, V> extends TreeMap<K, V> implements MMap<K, V> {
    private TreeMapEntry<K, V> _root;
    private int _size;
    public final MTreeMapKeySet<K, V> keys;
    @Override
    public MTreeMapKeySet<K, V> keys() {
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
        return new ImTreeMap<K, V>(this.comparator, ((this._root == null) ? (null) : (this._root.copyParent(null))), this._size);
    }
    @Override
    public ImTreeMap<K, V> im() {
        return new ImTreeMap<K, V>(this.comparator, this._root, this._size);
    }
    @Override
    public void assignImMap(final ImMap<K, V> imMap) {
        if(imMap instanceof ImTreeMap) {
            final ImTreeMap<K, V> m = ((ImTreeMap<K, V>)(((ImTreeMap)(imMap))));
            final TreeMapEntry<K, V> __tmp_0t_1u = m.root;
            this._root = ((__tmp_0t_1u == null) ? (null) : (__tmp_0t_1u.copyParent(null)));
            this._size = m.count;
        } else {
            this.clear();
            {
                final Iterator<Tuple<K, V>> __il__0f_1i = imMap.iterator();
                while(__il__0f_1i.hasNext()) {
                    final Tuple<K, V> _ = __il__0f_1i.next();
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
        this._size = ((int)(0));
        this._root = null;
    }
    @Override
    public MIterator<Tuple<K, V>> mutableIterator() {
        return ((MIterator<Tuple<K, V>>)(((MIterator)(MTreeMapIterator.<K, V>applyMapEntry(this, this.firstEntry())))));
    }
    @Override
    public void setKeyValue(final K key, final V value) {
        final F2<K, K, Integer> __comparator = this.comparator;
        TreeMapEntry<K, V> t = this._root;
        if(t == null) {
            this._root = new TreeMapEntry<K, V>(key, value, null);
            this._size = ((int)(1));
        } else {
            int cmp = 0;
            TreeMapEntry<K, V> parent = null;
            do {
                parent = ((TreeMapEntry<K, V>)(((TreeMapEntry)(t))));
                cmp = __comparator.apply(key, t.key);
                if(cmp < 0) {
                    t = t.left;
                } else {
                    if(cmp > 0) {
                        t = t.right;
                    } else {
                        t.value = value;
                        return ;
                    }
                }
            } while(t != null);
            final TreeMapEntry<K, V> e = new TreeMapEntry<K, V>(key, value, parent);
            if(cmp < 0) {
                if(parent == null) {
                    throw new NullPointerException();
                }
                parent.left = e;
            } else {
                if(parent == null) {
                    throw new NullPointerException();
                }
                parent.right = e;
            }
            fixAfterInsertionEntry(e);
            this._size++;
        }
    }
    @Override
    public V removeKey(final K key) {
        final TreeMapEntry<K, V> _ = entryForKey(key);
        if(_ != null) {
            return deleteEntry(((TreeMapEntry<K, V>)(((TreeMapEntry)(_)))));
        } else {
            return null;
        }
    }
    @Override
    public boolean removeItem(final Tuple<K, V> item) {
        return removeKey(item.a) != null;
    }
    public V deleteEntry(final TreeMapEntry<K, V> entry) {
        TreeMapEntry<K, V> p = entry;
        this._size--;
        if(p.left != null && p.right != null) {
            final TreeMapEntry<K, V> __tmp_2t_0n = p.next();
            if(__tmp_2t_0n == null) {
                throw new NullPointerException();
            }
            final TreeMapEntry<K, V> s = __tmp_2t_0n;
            p.key = s.key;
            p.value = s.value;
            p = s;
        }
        final TreeMapEntry<K, V> replacement = ((p.left != null) ? (p.left) : (p.right));
        if(replacement != null) {
            replacement.parent = p.parent;
            if(p.parent == null) {
                this._root = ((TreeMapEntry<K, V>)(((TreeMapEntry)(replacement))));
            } else {
                final TreeMapEntry<K, V> __tmp_4t_1fcbln = p.parent;
                if(__tmp_4t_1fcbln == null) {
                    throw new NullPointerException();
                }
                final TreeMapEntry<K, V> __tmp_4t_1fc = __tmp_4t_1fcbln.left;
                if(__tmp_4t_1fc != null && __tmp_4t_1fc.equals(p)) {
                    final TreeMapEntry<K, V> __tmp_4t_1ftln = p.parent;
                    if(__tmp_4t_1ftln == null) {
                        throw new NullPointerException();
                    }
                    __tmp_4t_1ftln.left = ((TreeMapEntry<K, V>)(((TreeMapEntry)(replacement))));
                } else {
                    final TreeMapEntry<K, V> __tmp_4t_1ffln = p.parent;
                    if(__tmp_4t_1ffln == null) {
                        throw new NullPointerException();
                    }
                    __tmp_4t_1ffln.right = ((TreeMapEntry<K, V>)(((TreeMapEntry)(replacement))));
                }
            }
            p.left = null;
            p.right = null;
            p.parent = null;
            if(p.color == MTreeMap.BLACK) {
                fixAfterDeletionEntry(((TreeMapEntry<K, V>)(((TreeMapEntry)(replacement)))));
            }
        } else {
            if(p.parent == null) {
                this._root = null;
            } else {
                if(p.color == MTreeMap.BLACK) {
                    fixAfterDeletionEntry(p);
                }
                final TreeMapEntry<K, V> g = p.parent;
                if(g != null) {
                    final TreeMapEntry<K, V> __tmp_4ff_2t_0c = g.left;
                    if(__tmp_4ff_2t_0c != null && __tmp_4ff_2t_0c.equals(p)) {
                        g.left = null;
                    } else {
                        final TreeMapEntry<K, V> __tmp_4ff_2t_0fc = g.right;
                        if(__tmp_4ff_2t_0fc != null && __tmp_4ff_2t_0fc.equals(p)) {
                            g.right = null;
                        }
                    }
                    p.parent = null;
                }
            }
        }
        return entry.value;
    }
    private void fixAfterInsertionEntry(final TreeMapEntry<K, V> entry) {
        entry.color = MTreeMap.RED;
        TreeMapEntry<K, V> x = entry;
        final TreeMapEntry<K, V> __tmp_2baln = x.parent;
        if(__tmp_2baln == null) {
            throw new NullPointerException();
        }
        while(x != null && (this._root == null || !(this._root.equals(x))) && __tmp_2baln.color == MTreeMap.RED) {
            final TreeMapEntry<K, V> __tmp_2_0n = x.parent;
            if(__tmp_2_0n == null) {
                throw new NullPointerException();
            }
            final TreeMapEntry<K, V> p = __tmp_2_0n;
            final TreeMapEntry<K, V> __tmp_2_1cbu = p.parent;
            final TreeMapEntry<K, V> __tmp_2_1c = ((__tmp_2_1cbu == null) ? (null) : (__tmp_2_1cbu.left));
            if(__tmp_2_1c != null && __tmp_2_1c.equals(p)) {
                final TreeMapEntry<K, V> __tmp_2_1t_0u = p.parent;
                final TreeMapEntry<K, V> y = ((__tmp_2_1t_0u == null) ? (null) : (__tmp_2_1t_0u.right));
                if(y != null && y.color == MTreeMap.RED) {
                    p.color = MTreeMap.BLACK;
                    y.color = MTreeMap.BLACK;
                    {
                        final TreeMapEntry<K, V> __tmp_2_1t_1t_2 = p.parent;
                        if(__tmp_2_1t_1t_2 != null) {
                            __tmp_2_1t_1t_2.color = MTreeMap.RED;
                        }
                    }
                    x = p.parent;
                } else {
                    final TreeMapEntry<K, V> __tmp_2_1t_1f_0c = p.right;
                    if(__tmp_2_1t_1f_0c != null && __tmp_2_1t_1f_0c.equals(x)) {
                        x = p;
                        rotateLeftP(((TreeMapEntry<K, V>)(((TreeMapEntry)(x)))));
                    }
                    final TreeMapEntry<K, V> pp = x.parent;
                    if(pp != null) {
                        pp.color = MTreeMap.BLACK;
                        {
                            final TreeMapEntry<K, V> __tmp_2_1t_1f_2t_1 = pp.parent;
                            if(__tmp_2_1t_1f_2t_1 != null) {
                                __tmp_2_1t_1f_2t_1.color = MTreeMap.RED;
                            }
                        }
                        rotateRightP(pp.parent);
                    }
                }
            } else {
                final TreeMapEntry<K, V> __tmp_2_1f_0u = p.parent;
                final TreeMapEntry<K, V> y = ((__tmp_2_1f_0u == null) ? (null) : (__tmp_2_1f_0u.left));
                if(y != null && y.color == MTreeMap.RED) {
                    p.color = MTreeMap.BLACK;
                    y.color = MTreeMap.BLACK;
                    {
                        final TreeMapEntry<K, V> __tmp_2_1f_1t_2 = p.parent;
                        if(__tmp_2_1f_1t_2 != null) {
                            __tmp_2_1f_1t_2.color = MTreeMap.RED;
                        }
                    }
                    x = p.parent;
                } else {
                    final TreeMapEntry<K, V> __tmp_2_1f_1f_0c = p.left;
                    if(__tmp_2_1f_1f_0c != null && __tmp_2_1f_1f_0c.equals(x)) {
                        x = p;
                        rotateRightP(((TreeMapEntry<K, V>)(((TreeMapEntry)(x)))));
                    }
                    final TreeMapEntry<K, V> pp = x.parent;
                    if(pp != null) {
                        pp.color = MTreeMap.BLACK;
                        {
                            final TreeMapEntry<K, V> __tmp_2_1f_1f_2t_1 = pp.parent;
                            if(__tmp_2_1f_1f_2t_1 != null) {
                                __tmp_2_1f_1f_2t_1.color = MTreeMap.RED;
                            }
                        }
                        rotateLeftP(pp.parent);
                    }
                }
            }
        }
        if(this._root != null) {
            this._root.color = MTreeMap.BLACK;
        }
    }
    private void fixAfterDeletionEntry(final TreeMapEntry<K, V> entry) {
        TreeMapEntry<K, V> x = entry;
        while(x != null && (this._root == null || !(this._root.equals(x))) && x.color == MTreeMap.BLACK) {
            final TreeMapEntry<K, V> __tmp_1_0cbu = x.parent;
            final TreeMapEntry<K, V> __tmp_1_0c = ((__tmp_1_0cbu == null) ? (null) : (__tmp_1_0cbu.left));
            if(__tmp_1_0c != null && __tmp_1_0c.equals(x)) {
                final TreeMapEntry<K, V> __tmp_1_0t_0u = x.parent;
                TreeMapEntry<K, V> sib = ((__tmp_1_0t_0u == null) ? (null) : (__tmp_1_0t_0u.right));
                if(sib != null && sib.color == MTreeMap.RED) {
                    sib.color = MTreeMap.BLACK;
                    {
                        final TreeMapEntry<K, V> __tmp_1_0t_1t_1 = x.parent;
                        if(__tmp_1_0t_1t_1 != null) {
                            __tmp_1_0t_1t_1.color = MTreeMap.RED;
                        }
                    }
                    rotateLeftP(x.parent);
                    final TreeMapEntry<K, V> __tmp_1_0t_1t_3u = x.parent;
                    sib = ((__tmp_1_0t_1t_3u == null) ? (null) : (__tmp_1_0t_1t_3u.right));
                }
                final TreeMapEntry<K, V> __tmp_1_0t_2caa = ((sib == null) ? (null) : (sib.left));
                final TreeMapEntry<K, V> __tmp_1_0t_2cba = ((sib == null) ? (null) : (sib.right));
                if(((__tmp_1_0t_2caa != null) ? (((sib == null) ? (null) : (sib.left)).color) : (MTreeMap.BLACK)) == MTreeMap.BLACK && ((__tmp_1_0t_2cba != null) ? (((sib == null) ? (null) : (sib.right)).color) : (MTreeMap.BLACK)) == MTreeMap.BLACK) {
                    if(sib != null) {
                        sib.color = MTreeMap.RED;
                    }
                    x = x.parent;
                } else {
                    final TreeMapEntry<K, V> __tmp_1_0t_2f_0ca = ((sib == null) ? (null) : (sib.right));
                    if(((__tmp_1_0t_2f_0ca != null) ? (((sib == null) ? (null) : (sib.right)).color) : (MTreeMap.BLACK)) == MTreeMap.BLACK) {
                        {
                            final TreeMapEntry<K, V> __tmp_1_0t_2f_0t_0 = ((sib == null) ? (null) : (sib.left));
                            if(__tmp_1_0t_2f_0t_0 != null) {
                                __tmp_1_0t_2f_0t_0.color = MTreeMap.BLACK;
                            }
                        }
                        if(sib != null) {
                            sib.color = MTreeMap.RED;
                        }
                        rotateRightP(sib);
                        final TreeMapEntry<K, V> __tmp_1_0t_2f_0t_3u = x.parent;
                        sib = ((__tmp_1_0t_2f_0t_3u == null) ? (null) : (__tmp_1_0t_2f_0t_3u.right));
                    }
                    if(sib != null) {
                        final TreeMapEntry<K, V> __tmp_1_0t_2f_1 = x.parent;
                        sib.color = ((__tmp_1_0t_2f_1 != null) ? (x.parent.color) : (MTreeMap.BLACK));
                    }
                    {
                        final TreeMapEntry<K, V> __tmp_1_0t_2f_2 = x.parent;
                        if(__tmp_1_0t_2f_2 != null) {
                            __tmp_1_0t_2f_2.color = MTreeMap.BLACK;
                        }
                    }
                    {
                        final TreeMapEntry<K, V> __tmp_1_0t_2f_3 = ((sib == null) ? (null) : (sib.right));
                        if(__tmp_1_0t_2f_3 != null) {
                            __tmp_1_0t_2f_3.color = MTreeMap.BLACK;
                        }
                    }
                    rotateLeftP(x.parent);
                    x = this._root;
                }
            } else {
                final TreeMapEntry<K, V> __tmp_1_0f_0u = x.parent;
                TreeMapEntry<K, V> sib = ((__tmp_1_0f_0u == null) ? (null) : (__tmp_1_0f_0u.left));
                if(sib != null && sib.color == MTreeMap.RED) {
                    sib.color = MTreeMap.BLACK;
                    {
                        final TreeMapEntry<K, V> __tmp_1_0f_1t_1 = x.parent;
                        if(__tmp_1_0f_1t_1 != null) {
                            __tmp_1_0f_1t_1.color = MTreeMap.RED;
                        }
                    }
                    rotateRightP(x.parent);
                    final TreeMapEntry<K, V> __tmp_1_0f_1t_3u = x.parent;
                    sib = ((__tmp_1_0f_1t_3u == null) ? (null) : (__tmp_1_0f_1t_3u.left));
                }
                final TreeMapEntry<K, V> __tmp_1_0f_2caa = ((sib == null) ? (null) : (sib.right));
                final TreeMapEntry<K, V> __tmp_1_0f_2cba = ((sib == null) ? (null) : (sib.left));
                if(((__tmp_1_0f_2caa != null) ? (((sib == null) ? (null) : (sib.right)).color) : (MTreeMap.BLACK)) == MTreeMap.BLACK && ((__tmp_1_0f_2cba != null) ? (((sib == null) ? (null) : (sib.left)).color) : (MTreeMap.BLACK)) == MTreeMap.BLACK) {
                    if(sib != null) {
                        sib.color = MTreeMap.RED;
                    }
                    x = x.parent;
                } else {
                    final TreeMapEntry<K, V> __tmp_1_0f_2f_0ca = ((sib == null) ? (null) : (sib.left));
                    if(((__tmp_1_0f_2f_0ca != null) ? (((sib == null) ? (null) : (sib.left)).color) : (MTreeMap.BLACK)) == MTreeMap.BLACK) {
                        {
                            final TreeMapEntry<K, V> __tmp_1_0f_2f_0t_0 = ((sib == null) ? (null) : (sib.right));
                            if(__tmp_1_0f_2f_0t_0 != null) {
                                __tmp_1_0f_2f_0t_0.color = MTreeMap.BLACK;
                            }
                        }
                        if(sib != null) {
                            sib.color = MTreeMap.RED;
                        }
                        rotateLeftP(sib);
                        final TreeMapEntry<K, V> __tmp_1_0f_2f_0t_3u = x.parent;
                        sib = ((__tmp_1_0f_2f_0t_3u == null) ? (null) : (__tmp_1_0f_2f_0t_3u.left));
                    }
                    if(sib != null) {
                        final TreeMapEntry<K, V> __tmp_1_0f_2f_1 = x.parent;
                        sib.color = ((__tmp_1_0f_2f_1 != null) ? (x.parent.color) : (MTreeMap.BLACK));
                    }
                    {
                        final TreeMapEntry<K, V> __tmp_1_0f_2f_2 = x.parent;
                        if(__tmp_1_0f_2f_2 != null) {
                            __tmp_1_0f_2f_2.color = MTreeMap.BLACK;
                        }
                    }
                    {
                        final TreeMapEntry<K, V> __tmp_1_0f_2f_3 = ((sib == null) ? (null) : (sib.left));
                        if(__tmp_1_0f_2f_3 != null) {
                            __tmp_1_0f_2f_3.color = MTreeMap.BLACK;
                        }
                    }
                    rotateRightP(x.parent);
                    x = this._root;
                }
            }
        }
        if(x != null) {
            x.color = MTreeMap.BLACK;
        }
    }
    private void rotateLeftP(final TreeMapEntry<K, V> p) {
        if(p != null) {
            final TreeMapEntry<K, V> __tmp_0t_0n = p.right;
            if(__tmp_0t_0n == null) {
                throw new NullPointerException();
            }
            final TreeMapEntry<K, V> r = __tmp_0t_0n;
            p.right = r.left;
            {
                final TreeMapEntry<K, V> __tmp_0t_2 = r.left;
                if(__tmp_0t_2 != null) {
                    __tmp_0t_2.parent = ((TreeMapEntry<K, V>)(((TreeMapEntry)(p))));
                }
            }
            r.parent = p.parent;
            if(p.parent == null) {
                this._root = r;
            } else {
                final TreeMapEntry<K, V> __tmp_0t_4fcaln = p.parent;
                if(__tmp_0t_4fcaln == null) {
                    throw new NullPointerException();
                }
                final TreeMapEntry<K, V> __tmp_0t_4fc = __tmp_0t_4fcaln.left;
                if(__tmp_0t_4fc != null && __tmp_0t_4fc.equals(p)) {
                    final TreeMapEntry<K, V> __tmp_0t_4ftln = p.parent;
                    if(__tmp_0t_4ftln == null) {
                        throw new NullPointerException();
                    }
                    __tmp_0t_4ftln.left = r;
                } else {
                    final TreeMapEntry<K, V> __tmp_0t_4ffln = p.parent;
                    if(__tmp_0t_4ffln == null) {
                        throw new NullPointerException();
                    }
                    __tmp_0t_4ffln.right = r;
                }
            }
            r.left = ((TreeMapEntry<K, V>)(((TreeMapEntry)(p))));
            p.parent = r;
        }
    }
    private void rotateRightP(final TreeMapEntry<K, V> p) {
        if(p != null) {
            final TreeMapEntry<K, V> __tmp_0t_0n = p.left;
            if(__tmp_0t_0n == null) {
                throw new NullPointerException();
            }
            final TreeMapEntry<K, V> l = __tmp_0t_0n;
            p.left = l.right;
            {
                final TreeMapEntry<K, V> __tmp_0t_2 = l.right;
                if(__tmp_0t_2 != null) {
                    __tmp_0t_2.parent = ((TreeMapEntry<K, V>)(((TreeMapEntry)(p))));
                }
            }
            l.parent = p.parent;
            if(p.parent == null) {
                this._root = l;
            } else {
                final TreeMapEntry<K, V> __tmp_0t_4fcaln = p.parent;
                if(__tmp_0t_4fcaln == null) {
                    throw new NullPointerException();
                }
                final TreeMapEntry<K, V> __tmp_0t_4fc = __tmp_0t_4fcaln.right;
                if(__tmp_0t_4fc != null && __tmp_0t_4fc.equals(p)) {
                    final TreeMapEntry<K, V> __tmp_0t_4ftln = p.parent;
                    if(__tmp_0t_4ftln == null) {
                        throw new NullPointerException();
                    }
                    __tmp_0t_4ftln.right = l;
                } else {
                    final TreeMapEntry<K, V> __tmp_0t_4ffln = p.parent;
                    if(__tmp_0t_4ffln == null) {
                        throw new NullPointerException();
                    }
                    __tmp_0t_4ffln.left = l;
                }
            }
            l.right = ((TreeMapEntry<K, V>)(((TreeMapEntry)(p))));
            p.parent = l;
        }
    }
    public Tuple<K, V> pollFirst() {
        final TreeMapEntry<K, V> entry = this.firstEntry();
        if(entry != null) {
            deleteEntry(((TreeMapEntry<K, V>)(((TreeMapEntry)(entry)))));
            return new Tuple<K, V>(entry.key, entry.value);
        } else {
            return null;
        }
    }
    public MTreeMap(final F2<K, K, Integer> comparator) {
        super(comparator);
        this._root = null;
        this._size = ((int)(0));
        this.keys = new MTreeMapKeySet<K, V>(this);
    }
    public String toString() {
        return "MTreeMap";
    }
    public V applyKeyOrUpdateWith(final K key, final F0<V> orUpdateWith) {
        final V __tmp = applyKey(key);
        if(__tmp != null) {
            return __tmp;
        } else {
            final V init = orUpdateWith.apply();
            setKeyValue(key, init);
            return init;
        }
    }
    public V modifyKeyBy(final K key, final F<V, V> by) {
        final V newObject = by.apply(applyKey(key));
        if(newObject == null) {
            removeKey(key);
        } else {
            setKeyValue(key, newObject);
        }
        return newObject;
    }
    @Override
    public void appendItem(final Tuple<K, V> item) {
        setKeyValue(item.a, item.b);
    }
    public void mutableFilterBy(final F<Tuple<K, V>, Boolean> by) {
        final MIterator<Tuple<K, V>> i = this.mutableIterator();
        while(i.hasNext()) {
            if(by.apply(i.next())) {
                i.remove();
            }
        }
    }
}