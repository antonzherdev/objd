package core.chain;

public class MMapDefault<K, V> implements MIterable<Tuple2<K, V>> {
    public final MMap<K, V> map;
    public final F<K, V> defaultFunc;
    @Override
    public int count() {
        return map.count();
    }
    @Override
    public Iterator<Tuple2<K, V>> iterator() {
        return map.iterator();
    }
    @Override
    public MIterator<Tuple2<K, V>> mutableIterator() {
        return map.mutableIterator();
    }
    public V applyKey(K key) {
        return map.objectForKeyOrUpdateWith(key, new F0<V>() {
            @Override
            public V f() {
                return defaultFunc.apply(key);
            }
        });
    }
    public Iterable<K> keys() {
        return map.keys();
    }
    public Iterable<V> values() {
        return map.values();
    }
    public boolean containsKey(K key) {
        return map.containsKey(key);
    }
    public void setKeyValue(K key,V value) {
        map.setKeyValue(key, value);
    }
    public V modifyKeyBy(K key,F<V, V> by) {
        ERROR: Unknown local value : V#G = <l>by\§V#G§ -> §V#G§\.<d>apply( = <MMapDefault#C<K#G, V#G>>self.<dI>apply(key = <l>key\§K#G§\)\§V#G§\)\§V#G§\;
        map.setKeyValue(key, value);
        return value;
    }
    @Override
    public void appendItem(Tuple2<K, V> item) {
        map.appendItem(item);
    }
    @Override
    public boolean removeItem(Tuple2<K, V> item) {
        return map.removeItem(item);
    }
    @Override
    public void clear() {
        map.clear();
    }
    @Override
    public ImMapDefault<K, V> im() {
        return new ImMapDefault<K, V>(map.im(), defaultFunc);
    }
    @Override
    public ImMapDefault<K, V> imCopy() {
        return new ImMapDefault<K, V>(map.imCopy(), defaultFunc);
    }
    public MMapDefault(MMap<K, V> map,F<K, V> defaultFunc) {
    }
    public void mutableFilterBy(F<T, Boolean> by) {
        ERROR: Unknown local i : MIterator#T<§T#G§> = <MIterable#T<T#G>>self.<dIa>mutableIterator\MIterator#T<§T#G§>\;
        ERROR: Unknown while(<l>i\MIterator#T<§T#G§>\.<rdIa>hasNext\bool\) {
    if(<l>by\§T#G§ -> bool\.<d>apply( = <l>i\MIterator#T<§T#G§>\.<rdIa>next\§T#G§\)\bool\) <l>i\MIterator#T<§T#G§>\.<dIa>remove\void\
};
    }
    @Override
    public T head() {
        ERROR: Unknown if(<Iterable#T<T#G>>self.<dI>isEmpty\bool\) return none<T#G>
else return some(<Iterable#T<T#G>>self.<dIa>iterator\Iterator#T<§T#G§>\.<dIa>next\§T#G§\)\§(T#G)?§\;
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
            public Boolean f(T item) {
                each.apply(item);
                return ERROR: Unknown True;
            }
        });
    }
    public void parForEach(P<T> each) {
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean f(T item) {
                DispatchQueue().default.asyncF(new P0() {
                    @Override
                    public void f() {
                        each.apply(item);
                    }
                });
                return ERROR: Unknown True;
            }
        });
    }
    public Chain<T> chain() {
        return Chain().chainWithCollection<T>(ERROR: Unknown <Traversable#T<T#G>>self);
    }
    public T findWhere(F<T, Boolean> where) {
        ERROR: Unknown local var ret : (T#G)? = none<T#G>;
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean f(T x) {
                ERROR: Unknown if(<l>where\§T#G§ -> bool\.<d>apply( = <l>x\§T#G§\)\bool\) {
    (<lm>ret\(§T#G§)?\ = some(<l>x\§T#G§\)\§(T#G)?§\)
    return False
}
else return True;
            }
        });
        return ret;
    }
    public boolean existsWhere(F<T, Boolean> where) {
        ERROR: Unknown local var ret : bool = False;
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean f(T x) {
                ERROR: Unknown if(<l>where\§T#G§ -> bool\.<d>apply( = <l>x\§T#G§\)\bool\) {
    (<lm>ret\bool\ = True)
    return False
}
else return True;
            }
        });
        return ret;
    }
    public boolean allConfirm(F<T, Boolean> confirm) {
        ERROR: Unknown local var ret : bool = True;
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean f(T x) {
                ERROR: Unknown if(!(<l>confirm\§T#G§ -> bool\.<d>apply( = <l>x\§T#G§\)\bool\)) {
    (<lm>ret\bool\ = False)
    return False
}
else return True;
            }
        });
        return ret;
    }
    public T head() {
        ERROR: Unknown local var ret : (T#G)? = ;
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean f(T on) {
                ERROR: Unknown (<lm>ret\(§T#G§)?\ = some(<l>on\§T#G§\)\§(T#G)?§\);
                return ERROR: Unknown False;
            }
        });
        return ret;
    }
    public C convertWithBuilder(Builder<T, C> builder) {
        forEach(new P<T>() {
            @Override
            public void f(T x) {
                builder.appendItem(x);
            }
        });
        return builder.build();
    }
    public void forEach(P<T> each) {
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean f(T item) {
                each.apply(item);
                return ERROR: Unknown True;
            }
        });
    }
    public void parForEach(P<T> each) {
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean f(T item) {
                DispatchQueue().default.asyncF(new P0() {
                    @Override
                    public void f() {
                        each.apply(item);
                    }
                });
                return ERROR: Unknown True;
            }
        });
    }
    public Chain<T> chain() {
        return Chain().chainWithCollection<T>(ERROR: Unknown <Traversable#T<T#G>>self);
    }
    public T findWhere(F<T, Boolean> where) {
        ERROR: Unknown local var ret : (T#G)? = none<T#G>;
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean f(T x) {
                ERROR: Unknown if(<l>where\§T#G§ -> bool\.<d>apply( = <l>x\§T#G§\)\bool\) {
    (<lm>ret\(§T#G§)?\ = some(<l>x\§T#G§\)\§(T#G)?§\)
    return False
}
else return True;
            }
        });
        return ret;
    }
    public boolean existsWhere(F<T, Boolean> where) {
        ERROR: Unknown local var ret : bool = False;
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean f(T x) {
                ERROR: Unknown if(<l>where\§T#G§ -> bool\.<d>apply( = <l>x\§T#G§\)\bool\) {
    (<lm>ret\bool\ = True)
    return False
}
else return True;
            }
        });
        return ret;
    }
    public boolean allConfirm(F<T, Boolean> confirm) {
        ERROR: Unknown local var ret : bool = True;
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean f(T x) {
                ERROR: Unknown if(!(<l>confirm\§T#G§ -> bool\.<d>apply( = <l>x\§T#G§\)\bool\)) {
    (<lm>ret\bool\ = False)
    return False
}
else return True;
            }
        });
        return ret;
    }
    public T head() {
        ERROR: Unknown local var ret : (T#G)? = ;
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean f(T on) {
                ERROR: Unknown (<lm>ret\(§T#G§)?\ = some(<l>on\§T#G§\)\§(T#G)?§\);
                return ERROR: Unknown False;
            }
        });
        return ret;
    }
    public C convertWithBuilder(Builder<T, C> builder) {
        forEach(new P<T>() {
            @Override
            public void f(T x) {
                builder.appendItem(x);
            }
        });
        return builder.build();
    }
}