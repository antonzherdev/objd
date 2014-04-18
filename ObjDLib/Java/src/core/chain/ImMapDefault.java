package core.chain;

public class ImMapDefault<K, V> implements ImIterable<Tuple2<K, V>> {
    public final ImMap<K, V> map;
    public final F<K, V> defaultFunc;
    public int count() {
        return map.count();
    }
    public Iterator<Tuple2<K, V>> iterator() {
        return map.iterator();
    }
    public V applyKey(K key) {
        ERROR: Unknown local __tmp : §(V#G)?§ = <ImMapDefault#C<K#G, V#G>>self.<eIU>map\ImMap#T<§K#G§, §V#G§>\.<rdIa>opt(key = <l>key\§K#G§\)\(§V#G§)?\;
        ERROR: Unknown if((<l>__tmp\§(V#G)?§\ != none<§V#G§>)) return <l>__tmp\§(V#G)?§\.get
else return <ImMapDefault#C<K#G, V#G>>self.<eIU>defaultFunc\§K#G§ -> §V#G§\.<d>apply( = <l>key\§K#G§\)\§V#G§\;
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
    public boolean isEqualMap(Map<K, V> map) {
        return ERROR: Unknown (<ImMapDefault#C<K#G, V#G>>self.<eIU>map\ImMap#T<§K#G§, §V#G§>\ == <l>map\Map#T<§K#G§, §V#G§>\);
    }
    public boolean isEqualMapDefault(ImMapDefault<K, V> mapDefault) {
        return ERROR: Unknown (<ImMapDefault#C<K#G, V#G>>self.<eIU>map\ImMap#T<§K#G§, §V#G§>\ == <l>mapDefault\ImMapDefault#C<§K#G§, §V#G§>\.<eIU>map\ImMap#T<§K#G§, §V#G§>\);
    }
    public int hash() {
        return map.hash();
    }
    public MMapDefault<K, V> mCopy() {
        return new MMapDefault<K, V>(map.mCopy(), defaultFunc);
    }
    public ImMapDefault(ImMap<K, V> map,F<K, V> defaultFunc) {
    }
    static final ClassType<ImMapDefault<K, V>> type;
    public T head() {
        ERROR: Unknown if(<Iterable#T<T#G>>self.<dI>isEmpty\bool\) return none<T#G>
else return some(<Iterable#T<T#G>>self.<dIa>iterator\Iterator#T<§T#G§>\.<dIa>next\§T#G§\)\§(T#G)?§\;
    }
    public boolean isEmpty() {
        return ERROR: Unknown !(<Iterable#T<T#G>>self.<dIa>iterator\Iterator#T<§T#G§>\.<dIa>hasNext\bool\);
    }
    public void forEach(F<T, Void> each) {
        ERROR: Unknown local i : Iterator#T<§T#G§> = <Iterable#T<T#G>>self.<dIa>iterator\Iterator#T<§T#G§>\;
        ERROR: Unknown while(<l>i\Iterator#T<§T#G§>\.<dIa>hasNext\bool\) <l>each\§T#G§ -> void\.<d>apply( = <l>i\Iterator#T<§T#G§>\.<dIa>next\§T#G§\)\void\;
    }
    public void parForEach(F<T, Void> each) {
        ERROR: Unknown local i : Iterator#T<§T#G§> = <Iterable#T<T#G>>self.<dIa>iterator\Iterator#T<§T#G§>\;
        ERROR: Unknown while(<l>i\Iterator#T<§T#G§>\.<dIa>hasNext\bool\) {
    local v : T#G = <l>i\Iterator#T<§T#G§>\.<dIa>next\§T#G§\
    <to>DispatchQueue\DispatchQueue#C.class\.<eIt>default\DispatchQueue#C\.<dI>async(f =  -> void = <l>each\§T#G§ -> void\.<d>apply( = <l>v\§T#G§\)\void\)\void\
};
    }
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
    public void forEach(F<T, Void> each) {
        goOn(ERROR: Unknown item : §T#G§ -> bool = {
    <l>each\§T#G§ -> void\.<d>apply( = <l>item\§T#G§\)\void\
    return True
});
    }
    public void parForEach(F<T, Void> each) {
        goOn(ERROR: Unknown item : §T#G§ -> bool = {
    <to>DispatchQueue\DispatchQueue#C.class\.<eIt>default\DispatchQueue#C\.<dI>async(f =  -> void = <l>each\§T#G§ -> void\.<d>apply( = <l>item\§T#G§\)\void\)\void\
    return True
});
    }
    public Chain<T> chain() {
        return Chain().chainWithCollection<T>(ERROR: Unknown <Traversable#T<T#G>>self);
    }
    public T findWhere(F<T, Boolean> where) {
        ERROR: Unknown local var ret : (T#G)? = none<T#G>;
        goOn(ERROR: Unknown x : §T#G§ -> bool = if(<l>where\§T#G§ -> bool\.<d>apply( = <l>x\§T#G§\)\bool\) {
    (<lm>ret\(§T#G§)?\ = some(<l>x\§T#G§\)\§(T#G)?§\)
    return False
}
else return True);
        return ret;
    }
    public boolean existsWhere(F<T, Boolean> where) {
        ERROR: Unknown local var ret : bool = False;
        goOn(ERROR: Unknown x : §T#G§ -> bool = if(<l>where\§T#G§ -> bool\.<d>apply( = <l>x\§T#G§\)\bool\) {
    (<lm>ret\bool\ = True)
    return False
}
else return True);
        return ret;
    }
    public boolean allConfirm(F<T, Boolean> confirm) {
        ERROR: Unknown local var ret : bool = True;
        goOn(ERROR: Unknown x : §T#G§ -> bool = if(!(<l>confirm\§T#G§ -> bool\.<d>apply( = <l>x\§T#G§\)\bool\)) {
    (<lm>ret\bool\ = False)
    return False
}
else return True);
        return ret;
    }
    public T head() {
        ERROR: Unknown local var ret : (T#G)? = ;
        goOn(ERROR: Unknown on : §T#G§ -> bool = {
    (<lm>ret\(§T#G§)?\ = some(<l>on\§T#G§\)\§(T#G)?§\)
    return False
});
        return ret;
    }
    public C convertWithBuilder(Builder<T, C> builder) {
        forEach(ERROR: Unknown x : §T#G§ -> void = <l>builder\Builder#T<§T#G§, C#G>\.<dIa>append(item = <l>x\§T#G§\)\void\);
        return builder.build();
    }
    public void forEach(F<T, Void> each) {
        goOn(ERROR: Unknown item : §T#G§ -> bool = {
    <l>each\§T#G§ -> void\.<d>apply( = <l>item\§T#G§\)\void\
    return True
});
    }
    public void parForEach(F<T, Void> each) {
        goOn(ERROR: Unknown item : §T#G§ -> bool = {
    <to>DispatchQueue\DispatchQueue#C.class\.<eIt>default\DispatchQueue#C\.<dI>async(f =  -> void = <l>each\§T#G§ -> void\.<d>apply( = <l>item\§T#G§\)\void\)\void\
    return True
});
    }
    public Chain<T> chain() {
        return Chain().chainWithCollection<T>(ERROR: Unknown <Traversable#T<T#G>>self);
    }
    public T findWhere(F<T, Boolean> where) {
        ERROR: Unknown local var ret : (T#G)? = none<T#G>;
        goOn(ERROR: Unknown x : §T#G§ -> bool = if(<l>where\§T#G§ -> bool\.<d>apply( = <l>x\§T#G§\)\bool\) {
    (<lm>ret\(§T#G§)?\ = some(<l>x\§T#G§\)\§(T#G)?§\)
    return False
}
else return True);
        return ret;
    }
    public boolean existsWhere(F<T, Boolean> where) {
        ERROR: Unknown local var ret : bool = False;
        goOn(ERROR: Unknown x : §T#G§ -> bool = if(<l>where\§T#G§ -> bool\.<d>apply( = <l>x\§T#G§\)\bool\) {
    (<lm>ret\bool\ = True)
    return False
}
else return True);
        return ret;
    }
    public boolean allConfirm(F<T, Boolean> confirm) {
        ERROR: Unknown local var ret : bool = True;
        goOn(ERROR: Unknown x : §T#G§ -> bool = if(!(<l>confirm\§T#G§ -> bool\.<d>apply( = <l>x\§T#G§\)\bool\)) {
    (<lm>ret\bool\ = False)
    return False
}
else return True);
        return ret;
    }
    public T head() {
        ERROR: Unknown local var ret : (T#G)? = ;
        goOn(ERROR: Unknown on : §T#G§ -> bool = {
    (<lm>ret\(§T#G§)?\ = some(<l>on\§T#G§\)\§(T#G)?§\)
    return False
});
        return ret;
    }
    public C convertWithBuilder(Builder<T, C> builder) {
        forEach(ERROR: Unknown x : §T#G§ -> void = <l>builder\Builder#T<§T#G§, C#G>\.<dIa>append(item = <l>x\§T#G§\)\void\);
        return builder.build();
    }
}