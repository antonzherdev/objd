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
        ERROR: Unknown local var p : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <TreeMap#C<K#G, V#G>>self.<dIa>root\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
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
        ERROR: Unknown local var p : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <TreeMap#C<K#G, V#G>>self.<dIa>root\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
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
        ERROR: Unknown local var p : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <TreeMap#C<K#G, V#G>>self.<dIa>root\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
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
        ERROR: Unknown local var p : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <TreeMap#C<K#G, V#G>>self.<dIa>root\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
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
        ERROR: Unknown local var p : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <TreeMap#C<K#G, V#G>>self.<dIa>root\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
        if(p != null) {
            ERROR: Unknown while((<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>)) (<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\);
        }
        return p;
    }
    public TreeMap(F2<K, K, Integer> comparator) {
        this.comparator = comparator;
    }
    public ImMap<K, V> addItem(Tuple2<K, V> item) {
        ERROR: Unknown local builder : HashMapBuilder#C<§K#G§, §V#G§> = <to>HashMapBuilder\HashMapBuilder#C.class\.<tcI>apply\HashMapBuilder#C<§K#G§, §V#G§>\;
        builder.appendAllItems(this);
        builder.appendItem(item);
        return builder.build();
    }
    @Override
    public MMap<K, V> mCopy() {
        ERROR: Unknown local m : MHashMap#C<§K#G§, §V#G§> = <to>MHashMap\MHashMap#C.class\.<tcI>apply\MHashMap#C<§K#G§, §V#G§>\;
        m.assignImMap(this);
        return m;
    }
    public V getKeyOrValue(K key,V orValue) {
        ERROR: Unknown local __tmp : §(V#G)?§ = <Map#T<K#G, V#G>>self.<dIa>opt(key = <l>key\§K#G§\)\(§V#G§)?\;
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
        ERROR: Unknown val __tmp : ^(^bool)?
{
    local _ : §(V#G)¿§ = <Map#T<K#G, V#G>>self.<dIa>opt(key = <l>key\§K#G§\)\(§V#G§)?\
    if((<l>_\§(V#G)¿§\ != none<§V#G§>)) (<l>__tmp\^(^bool)?\ = some((<l>_\§(V#G)¿§\ == <l>value\§V#G§\))\(^bool)?\)
else (<l>__tmp\^(^bool)?\ = none<^bool>)
};
        if(__tmp != null) {
            return __tmp;
        } else {
            return ERROR: Unknown False;
        }
    }
    public int count() {
        ERROR: Unknown local i : Iterator#T<§T#G§> = <Iterable#T<T#G>>self.<dIa>iterator\Iterator#T<§T#G§>\;
        ERROR: Unknown local var n : uint = 0.cast<uint>;
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
        ERROR: Unknown local i : Iterator#T<§T#G§> = <Iterable#T<T#G>>self.<dIa>iterator\Iterator#T<§T#G§>\;
        ERROR: Unknown while(<l>i\Iterator#T<§T#G§>\.<dIa>hasNext\bool\) <l>each\§T#G§ -> void\.<d>apply( = <l>i\Iterator#T<§T#G§>\.<dIa>next\§T#G§\)\void\;
    }
    @Override
    public void parForEach(P<T> each) {
        ERROR: Unknown local i : Iterator#T<§T#G§> = <Iterable#T<T#G>>self.<dIa>iterator\Iterator#T<§T#G§>\;
        ERROR: Unknown while(<l>i\Iterator#T<§T#G§>\.<dIa>hasNext\bool\) {
    local v : T#G = <l>i\Iterator#T<§T#G§>\.<dIa>next\§T#G§\
    <to>DispatchQueue\DispatchQueue#C.class\.<eIt>default\DispatchQueue#C\.<dIb>async(f =  -> void = <l>each\§T#G§ -> void\.<d>apply( = <l>v\§T#G§\)\void\)\void\
};
    }
    @Override
    public boolean goOn(F<T, Boolean> on) {
        ERROR: Unknown local i : Iterator#T<§T#G§> = <Iterable#T<T#G>>self.<dIa>iterator\Iterator#T<§T#G§>\;
        ERROR: Unknown while(<l>i\Iterator#T<§T#G§>\.<dIa>hasNext\bool\) if(!(<l>on\§T#G§ -> bool\.<d>apply( = <l>i\Iterator#T<§T#G§>\.<dIa>next\§T#G§\)\bool\)) return False;
        return ERROR: Unknown True;
    }
    public boolean containsItem(T item) {
        ERROR: Unknown local i : Iterator#T<§T#G§> = <Iterable#T<T#G>>self.<dIa>iterator\Iterator#T<§T#G§>\;
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
        ERROR: Unknown local var ret : (T#G)? = none<T#G>;
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
        ERROR: Unknown local var ret : bool = False;
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
        ERROR: Unknown local var ret : bool = True;
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
        ERROR: Unknown local var ret : (T#G)? = ;
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
        ERROR: Unknown local arr : MArray#C<§T#G§> = <to>MArray\MArray#C.class\.<tcI>apply\MArray#C<§T#G§>\;
        forEach(new P<T>() {
            @Override
            public void apply(T item) {
                arr.appendItem(item);
            }
        });
        return arr;
    }
    public int count() {
        ERROR: Unknown local i : Iterator#T<§T#G§> = <Iterable#T<T#G>>self.<dIa>iterator\Iterator#T<§T#G§>\;
        ERROR: Unknown local var n : uint = 0.cast<uint>;
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
        ERROR: Unknown local i : Iterator#T<§T#G§> = <Iterable#T<T#G>>self.<dIa>iterator\Iterator#T<§T#G§>\;
        ERROR: Unknown while(<l>i\Iterator#T<§T#G§>\.<dIa>hasNext\bool\) <l>each\§T#G§ -> void\.<d>apply( = <l>i\Iterator#T<§T#G§>\.<dIa>next\§T#G§\)\void\;
    }
    @Override
    public void parForEach(P<T> each) {
        ERROR: Unknown local i : Iterator#T<§T#G§> = <Iterable#T<T#G>>self.<dIa>iterator\Iterator#T<§T#G§>\;
        ERROR: Unknown while(<l>i\Iterator#T<§T#G§>\.<dIa>hasNext\bool\) {
    local v : T#G = <l>i\Iterator#T<§T#G§>\.<dIa>next\§T#G§\
    <to>DispatchQueue\DispatchQueue#C.class\.<eIt>default\DispatchQueue#C\.<dIb>async(f =  -> void = <l>each\§T#G§ -> void\.<d>apply( = <l>v\§T#G§\)\void\)\void\
};
    }
    @Override
    public boolean goOn(F<T, Boolean> on) {
        ERROR: Unknown local i : Iterator#T<§T#G§> = <Iterable#T<T#G>>self.<dIa>iterator\Iterator#T<§T#G§>\;
        ERROR: Unknown while(<l>i\Iterator#T<§T#G§>\.<dIa>hasNext\bool\) if(!(<l>on\§T#G§ -> bool\.<d>apply( = <l>i\Iterator#T<§T#G§>\.<dIa>next\§T#G§\)\bool\)) return False;
        return ERROR: Unknown True;
    }
    public boolean containsItem(T item) {
        ERROR: Unknown local i : Iterator#T<§T#G§> = <Iterable#T<T#G>>self.<dIa>iterator\Iterator#T<§T#G§>\;
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
        ERROR: Unknown local var ret : (T#G)? = none<T#G>;
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
        ERROR: Unknown local var ret : bool = False;
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
        ERROR: Unknown local var ret : bool = True;
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
        ERROR: Unknown local var ret : (T#G)? = ;
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
        ERROR: Unknown local arr : MArray#C<§T#G§> = <to>MArray\MArray#C.class\.<tcI>apply\MArray#C<§T#G§>\;
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
        ERROR: Unknown local var ret : (T#G)? = none<T#G>;
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
        ERROR: Unknown local var ret : bool = False;
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
        ERROR: Unknown local var ret : bool = True;
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
        ERROR: Unknown local var ret : (T#G)? = ;
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