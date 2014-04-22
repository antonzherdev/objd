package core.chain;

public class TreeMap<K, V> implements ImMap<K, V> {
    public static final int BLACK = ERROR: Unknown 0;
    public static final int RED = ERROR: Unknown 1;
    public final F2<K, K, Integer> comparator;
    public final TreeMapValues<V> values = new TreeMapValues<V>(this);
    @Override
    public V applyKey(K key) {
        return ERROR: Unknown {
    local __tmp : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <TreeMap#C<K#G, V#G>>self.<dI>entryFor(key = <l>key\§K#G§\)\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    if((<l>__tmp\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == none<^TreeMapEntry#C<§K#G§, §V#G§>>)) throw "Not null"
else <l>__tmp\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
}.value;
    }
    @Override
    public V optKey(K key) {
        return ERROR: Unknown <TreeMap#C<K#G, V#G>>self.<dI>entryFor(key = <l>key\§K#G§\)\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUm>value\§V#G§\;
    }
    public abstract TreeMapEntry<K, V> root();
    @Override
    public boolean isEmpty() {
        return this.root() == null;
    }
    public TreeMapEntry<K, V> entryForKey(K key) {
        TreeMapEntry<K, V> p = this.root();
        ERROR: Unknown while((<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>)) {
    local cmp : int = <TreeMap#C<K#G, V#G>>self.<eIU>comparator\(§K#G§, §K#G§) -> int\.<d>apply( = <l>key\§K#G§\,  = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUm>key\§K#G§\)\int\
    if((<l>cmp\int\ < 0)) (<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
else if((<l>cmp\int\ > 0)) (<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
else break
};
        return p;
    }
    @Override
    public abstract TreeMapKeySet<K> keys();
    @Override
    public Iterator<Tuple2<K, V>> iterator() {
        return TreeMapIterator().applyMapEntry<K, V>(this, this.firstEntry());
    }
    public TreeMapIterator<K, V> iteratorHigherThanKey(K key) {
        return TreeMapIterator().applyMapEntry<K, V>(this, higherEntryThanKey(key));
    }
    public TreeMapEntry<K, V> firstEntry() {
        TreeMapEntry<K, V> p = this.root();
        if(p != null) {
            ERROR: Unknown while((<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>)) (<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\);
        }
        return p;
    }
    public K firstKey() {
        return ERROR: Unknown <TreeMap#C<K#G, V#G>>self.<dI>firstEntry\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUm>key\§K#G§\;
    }
    public K lastKey() {
        return ERROR: Unknown <TreeMap#C<K#G, V#G>>self.<dp>lastEntry\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUm>key\§K#G§\;
    }
    public K lowerKeyThanKey(K key) {
        return ERROR: Unknown <TreeMap#C<K#G, V#G>>self.<dp>lowerEntryThan(key = <l>key\§K#G§\)\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUm>key\§K#G§\;
    }
    public K higherKeyThanKey(K key) {
        return ERROR: Unknown <TreeMap#C<K#G, V#G>>self.<dp>higherEntryThan(key = <l>key\§K#G§\)\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUm>key\§K#G§\;
    }
    private TreeMapEntry<K, V> lowerEntryThanKey(K key) {
        TreeMapEntry<K, V> p = this.root();
        ERROR: Unknown while((<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>)) {
    local cmp : int = <TreeMap#C<K#G, V#G>>self.<eIU>comparator\(§K#G§, §K#G§) -> int\.<d>apply( = <l>key\§K#G§\,  = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUm>key\§K#G§\)\int\
    if((<l>cmp\int\ > 0)) {
    if((<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>)) (<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
else return <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\
}
else {
    if((<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>)) {
    (<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
}
else {
    local var parent : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    local var ch : (^TreeMapEntry#C<§K#G§, §V#G§>)¿ = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\
    while(((<lm>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>) && {
    local __tmp_1_1_0_2 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    return ((<l>__tmp_1_1_0_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>) && (<l>__tmp_1_1_0_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == <lm>ch\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\))
})) {
    (<lm>ch\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\)
    (<lm>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
}
    return <lm>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
}
}
};
        return null;
    }
    private TreeMapEntry<K, V> higherEntryThanKey(K key) {
        TreeMapEntry<K, V> p = this.root();
        ERROR: Unknown while((<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>)) {
    local cmp : int = <TreeMap#C<K#G, V#G>>self.<eIU>comparator\(§K#G§, §K#G§) -> int\.<d>apply( = <l>key\§K#G§\,  = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUm>key\§K#G§\)\int\
    if((<l>cmp\int\ < 0)) {
    if((<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>)) (<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
else return <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\
}
else {
    if((<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>)) {
    (<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
}
else {
    local var parent : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    local var ch : (^TreeMapEntry#C<§K#G§, §V#G§>)¿ = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\
    while(((<lm>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>) && {
    local __tmp_1_1_0_2 : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <lm>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
    return ((<l>__tmp_1_1_0_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>) && (<l>__tmp_1_1_0_2\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == <lm>ch\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\))
})) {
    (<lm>ch\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\)
    (<lm>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUmw>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
}
    return <lm>parent\(^TreeMapEntry#C<§K#G§, §V#G§>)?\
}
}
};
        return null;
    }
    private TreeMapEntry<K, V> lastEntry() {
        TreeMapEntry<K, V> p = this.root();
        if(p != null) {
            ERROR: Unknown while((<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>)) (<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\);
        }
        return p;
    }
    public TreeMap(F2<K, K, Integer> comparator) {
        this.comparator = comparator;
    }
    public ImMap<K, V> addItem(Tuple2<K, V> item) {
        HashMapBuilder<K, V> builder = new HashMapBuilder<K, V>();
        builder.appendAllItems(this);
        builder.appendItem(item);
        return builder.build();
    }
    @Override
    public MMap<K, V> mCopy() {
        MHashMap<K, V> m = new MHashMap<K, V>();
        m.assignImMap(this);
        return m;
    }
    public V getKeyOrValue(K key,V orValue) {
        V __tmp = optKey(key);
        if(__tmp != null) {
            return __tmp;
        } else {
            return orValue;
        }
    }
    public boolean containsKey(K key) {
        return optKey(key) != null;
    }
    public boolean isValueEqualKeyValue(K key,V value) {
        Boolean __tmp;
        {
            V _ = optKey(key);
            if(_ != null) {
                __tmp = _.equals(value);
            } else {
                __tmp = null;
            }
        }
        if(__tmp != null) {
            return __tmp;
        } else {
            return ERROR: Unknown False;
        }
    }
    public int count() {
        Iterator<T> i = this.iterator();
        int n = ERROR: Unknown 0.cast<uint>;
        ERROR: Unknown while(<l>i\Iterator#T<§T#G§>\.<dIa>hasNext\bool\) {
    <l>i\Iterator#T<§T#G§>\.<dIa>next\§T#G§\
    <lm>n\uint\++
};
        return n;
    }
    @Override
    public T head() {
        if(this.isEmpty()) {
            return null;
        } else {
            return this.iterator().next();
        }
    }
    @Override
    public void forEach(P<T> each) {
        Iterator<T> i = this.iterator();
        ERROR: Unknown while(<l>i\Iterator#T<§T#G§>\.<dIa>hasNext\bool\) <l>each\§T#G§ -> void\.<d>apply( = <l>i\Iterator#T<§T#G§>\.<dIa>next\§T#G§\)\void\;
    }
    @Override
    public void parForEach(P<T> each) {
        Iterator<T> i = this.iterator();
        ERROR: Unknown while(<l>i\Iterator#T<§T#G§>\.<dIa>hasNext\bool\) {
    local v : T#G = <l>i\Iterator#T<§T#G§>\.<dIa>next\§T#G§\
    <to>DispatchQueue\DispatchQueue#C.class\.<eIt>default\DispatchQueue#C\.<dIb>async(f =  -> void = <l>each\§T#G§ -> void\.<d>apply( = <l>v\§T#G§\)\void\)\void\
};
    }
    @Override
    public boolean goOn(F<T, Boolean> on) {
        Iterator<T> i = this.iterator();
        ERROR: Unknown while(<l>i\Iterator#T<§T#G§>\.<dIa>hasNext\bool\) if(!(<l>on\§T#G§ -> bool\.<d>apply( = <l>i\Iterator#T<§T#G§>\.<dIa>next\§T#G§\)\bool\)) return False;
        return ERROR: Unknown True;
    }
    public boolean containsItem(T item) {
        Iterator<T> i = this.iterator();
        ERROR: Unknown while(<l>i\Iterator#T<§T#G§>\.<dIa>hasNext\bool\) if((<l>i\Iterator#T<§T#G§>\.<dIa>next\§T#G§\ == <l>i\Iterator#T<§T#G§>\)) return True;
        return ERROR: Unknown False;
    }
    public void forEach(P<T> each) {
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean apply(T item) {
                each.apply(item);
                return ERROR: Unknown True;
            }
        });
    }
    public void parForEach(P<T> each) {
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean apply(T item) {
                DispatchQueue().default.asyncF(new P0() {
                    @Override
                    public void apply() {
                        each.apply(item);
                    }
                });
                return ERROR: Unknown True;
            }
        });
    }
    public Chain<T> chain() {
        return Chain().chainWithCollection<T>(this);
    }
    public T findWhere(F<T, Boolean> where) {
        T ret = null;
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean apply(T x) {
                if(where.apply(x)) {
                    ret = x;
                    return ERROR: Unknown False;
                } else {
                    return ERROR: Unknown True;
                }
            }
        });
        return ret;
    }
    public boolean existsWhere(F<T, Boolean> where) {
        boolean ret = ERROR: Unknown False;
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean apply(T x) {
                if(where.apply(x)) {
                    ret = ERROR: Unknown True;
                    return ERROR: Unknown False;
                } else {
                    return ERROR: Unknown True;
                }
            }
        });
        return ret;
    }
    public boolean allConfirm(F<T, Boolean> confirm) {
        boolean ret = ERROR: Unknown True;
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean apply(T x) {
                if(ERROR: Unknown !(<l>confirm\§T#G§ -> bool\.<d>apply( = <l>x\§T#G§\)\bool\)) {
                    ret = ERROR: Unknown False;
                    return ERROR: Unknown False;
                } else {
                    return ERROR: Unknown True;
                }
            }
        });
        return ret;
    }
    public T head() {
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean apply(T on) {
                ret = on;
                return ERROR: Unknown False;
            }
        });
        return ret;
    }
    public C convertWithBuilder(Builder<T, C> builder) {
        forEach(new P<T>() {
            @Override
            public void apply(T x) {
                builder.appendItem(x);
            }
        });
        return builder.build();
    }
    @Override
    public MIterable<T> mCopy() {
        MArray<T> arr = new MArray<T>();
        forEach(new P<T>() {
            @Override
            public void apply(T item) {
                arr.appendItem(item);
            }
        });
        return arr;
    }
    public int count() {
        Iterator<T> i = this.iterator();
        int n = ERROR: Unknown 0.cast<uint>;
        ERROR: Unknown while(<l>i\Iterator#T<§T#G§>\.<dIa>hasNext\bool\) {
    <l>i\Iterator#T<§T#G§>\.<dIa>next\§T#G§\
    <lm>n\uint\++
};
        return n;
    }
    @Override
    public T head() {
        if(this.isEmpty()) {
            return null;
        } else {
            return this.iterator().next();
        }
    }
    @Override
    public void forEach(P<T> each) {
        Iterator<T> i = this.iterator();
        ERROR: Unknown while(<l>i\Iterator#T<§T#G§>\.<dIa>hasNext\bool\) <l>each\§T#G§ -> void\.<d>apply( = <l>i\Iterator#T<§T#G§>\.<dIa>next\§T#G§\)\void\;
    }
    @Override
    public void parForEach(P<T> each) {
        Iterator<T> i = this.iterator();
        ERROR: Unknown while(<l>i\Iterator#T<§T#G§>\.<dIa>hasNext\bool\) {
    local v : T#G = <l>i\Iterator#T<§T#G§>\.<dIa>next\§T#G§\
    <to>DispatchQueue\DispatchQueue#C.class\.<eIt>default\DispatchQueue#C\.<dIb>async(f =  -> void = <l>each\§T#G§ -> void\.<d>apply( = <l>v\§T#G§\)\void\)\void\
};
    }
    @Override
    public boolean goOn(F<T, Boolean> on) {
        Iterator<T> i = this.iterator();
        ERROR: Unknown while(<l>i\Iterator#T<§T#G§>\.<dIa>hasNext\bool\) if(!(<l>on\§T#G§ -> bool\.<d>apply( = <l>i\Iterator#T<§T#G§>\.<dIa>next\§T#G§\)\bool\)) return False;
        return ERROR: Unknown True;
    }
    public boolean containsItem(T item) {
        Iterator<T> i = this.iterator();
        ERROR: Unknown while(<l>i\Iterator#T<§T#G§>\.<dIa>hasNext\bool\) if((<l>i\Iterator#T<§T#G§>\.<dIa>next\§T#G§\ == <l>i\Iterator#T<§T#G§>\)) return True;
        return ERROR: Unknown False;
    }
    public void forEach(P<T> each) {
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean apply(T item) {
                each.apply(item);
                return ERROR: Unknown True;
            }
        });
    }
    public void parForEach(P<T> each) {
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean apply(T item) {
                DispatchQueue().default.asyncF(new P0() {
                    @Override
                    public void apply() {
                        each.apply(item);
                    }
                });
                return ERROR: Unknown True;
            }
        });
    }
    public Chain<T> chain() {
        return Chain().chainWithCollection<T>(this);
    }
    public T findWhere(F<T, Boolean> where) {
        T ret = null;
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean apply(T x) {
                if(where.apply(x)) {
                    ret = x;
                    return ERROR: Unknown False;
                } else {
                    return ERROR: Unknown True;
                }
            }
        });
        return ret;
    }
    public boolean existsWhere(F<T, Boolean> where) {
        boolean ret = ERROR: Unknown False;
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean apply(T x) {
                if(where.apply(x)) {
                    ret = ERROR: Unknown True;
                    return ERROR: Unknown False;
                } else {
                    return ERROR: Unknown True;
                }
            }
        });
        return ret;
    }
    public boolean allConfirm(F<T, Boolean> confirm) {
        boolean ret = ERROR: Unknown True;
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean apply(T x) {
                if(ERROR: Unknown !(<l>confirm\§T#G§ -> bool\.<d>apply( = <l>x\§T#G§\)\bool\)) {
                    ret = ERROR: Unknown False;
                    return ERROR: Unknown False;
                } else {
                    return ERROR: Unknown True;
                }
            }
        });
        return ret;
    }
    public T head() {
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean apply(T on) {
                ret = on;
                return ERROR: Unknown False;
            }
        });
        return ret;
    }
    public C convertWithBuilder(Builder<T, C> builder) {
        forEach(new P<T>() {
            @Override
            public void apply(T x) {
                builder.appendItem(x);
            }
        });
        return builder.build();
    }
    public MTraversable<T> mCopy() {
        MArray<T> arr = new MArray<T>();
        forEach(new P<T>() {
            @Override
            public void apply(T item) {
                arr.appendItem(item);
            }
        });
        return arr;
    }
    public void forEach(P<T> each) {
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean apply(T item) {
                each.apply(item);
                return ERROR: Unknown True;
            }
        });
    }
    public void parForEach(P<T> each) {
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean apply(T item) {
                DispatchQueue().default.asyncF(new P0() {
                    @Override
                    public void apply() {
                        each.apply(item);
                    }
                });
                return ERROR: Unknown True;
            }
        });
    }
    public Chain<T> chain() {
        return Chain().chainWithCollection<T>(this);
    }
    public T findWhere(F<T, Boolean> where) {
        T ret = null;
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean apply(T x) {
                if(where.apply(x)) {
                    ret = x;
                    return ERROR: Unknown False;
                } else {
                    return ERROR: Unknown True;
                }
            }
        });
        return ret;
    }
    public boolean existsWhere(F<T, Boolean> where) {
        boolean ret = ERROR: Unknown False;
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean apply(T x) {
                if(where.apply(x)) {
                    ret = ERROR: Unknown True;
                    return ERROR: Unknown False;
                } else {
                    return ERROR: Unknown True;
                }
            }
        });
        return ret;
    }
    public boolean allConfirm(F<T, Boolean> confirm) {
        boolean ret = ERROR: Unknown True;
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean apply(T x) {
                if(ERROR: Unknown !(<l>confirm\§T#G§ -> bool\.<d>apply( = <l>x\§T#G§\)\bool\)) {
                    ret = ERROR: Unknown False;
                    return ERROR: Unknown False;
                } else {
                    return ERROR: Unknown True;
                }
            }
        });
        return ret;
    }
    public T head() {
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean apply(T on) {
                ret = on;
                return ERROR: Unknown False;
            }
        });
        return ret;
    }
    public C convertWithBuilder(Builder<T, C> builder) {
        forEach(new P<T>() {
            @Override
            public void apply(T x) {
                builder.appendItem(x);
            }
        });
        return builder.build();
    }
}