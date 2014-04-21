package core.chain;

public class MMapDefault<K, V> implements MIterable<Tuple2<K, V>> {
    public final MMap<K, V> map;
    public final F<K, V> defaultFunc;
    @Override
    public int count() {
        return this.map.count();
    }
    @Override
    public Iterator<Tuple2<K, V>> iterator() {
        return this.map.iterator();
    }
    @Override
    public MIterator<Tuple2<K, V>> mutableIterator() {
        return this.map.mutableIterator();
    }
    public V applyKey(K key) {
        return this.map.objectForKeyOrUpdateWith(key, new F0<V>() {
            @Override
            public V apply() {
                return MMapDefault.this.defaultFunc.apply(key);
            }
        });
    }
    public Iterable<K> keys() {
        return this.map.keys();
    }
    public Iterable<V> values() {
        return this.map.values();
    }
    public boolean containsKey(K key) {
        return this.map.containsKey(key);
    }
    public void setKeyValue(K key,V value) {
        this.map.setKeyValue(key, value);
    }
    public V modifyKeyBy(K key,F<V, V> by) {
        ERROR: Unknown local value : V#G = <l>by\§V#G§ -> §V#G§\.<d>apply( = <MMapDefault#C<K#G, V#G>>self.<dI>apply(key = <l>key\§K#G§\)\§V#G§\)\§V#G§\;
        this.map.setKeyValue(key, value);
        return value;
    }
    @Override
    public void appendItem(Tuple2<K, V> item) {
        this.map.appendItem(item);
    }
    @Override
    public boolean removeItem(Tuple2<K, V> item) {
        return this.map.removeItem(item);
    }
    @Override
    public void clear() {
        this.map.clear();
    }
    @Override
    public ImMapDefault<K, V> im() {
        return new ImMapDefault<K, V>(this.map.im(), this.defaultFunc);
    }
    @Override
    public ImMapDefault<K, V> imCopy() {
        return new ImMapDefault<K, V>(this.map.imCopy(), this.defaultFunc);
    }
    public MMapDefault(MMap<K, V> map,F<K, V> defaultFunc) {
        this.map = map;
        this.defaultFunc = defaultFunc;
    }
    public void mutableFilterBy(F<T, Boolean> by) {
        ERROR: Unknown local i : MIterator#T<§T#G§> = <MIterable#T<T#G>>self.<dIa>mutableIterator\MIterator#T<§T#G§>\;
        ERROR: Unknown while(<l>i\MIterator#T<§T#G§>\.<rdIa>hasNext\bool\) {
    if(<l>by\§T#G§ -> bool\.<d>apply( = <l>i\MIterator#T<§T#G§>\.<rdIa>next\§T#G§\)\bool\) <l>i\MIterator#T<§T#G§>\.<dIa>remove\void\
};
    }
    @Override
    public T head() {
        if(this.isEmpty()) {
            return null;
        } else {
            return this.iterator().next();
        }
    }
    public boolean isEmpty() {
        return ERROR: Unknown !(<Iterable#T<T#G>>self.<dIa>iterator\Iterator#T<§T#G§>\.<dIa>hasNext\bool\);
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
    <to>DispatchQueue\DispatchQueue#C.class\.<eIt>default\DispatchQueue#C\.<dI>async(f =  -> void = <l>each\§T#G§ -> void\.<d>apply( = <l>v\§T#G§\)\void\)\void\
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