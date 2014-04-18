package core.chain;

public class MMapDefault<K, V> implements MIterable<Tuple2<K, V>> {
    public final MMap<K, V> map;
    public final F<K, V> defaultFunc;
    public int count() {
        return ERROR: Unknown <MMapDefault#C<K#G, V#G>>self.<eIU>map\MMap#T<§K#G§, §V#G§>\.<rdI>count\uint\;
    }
    public Iterator<Tuple2<K, V>> iterator() {
        return ERROR: Unknown <MMapDefault#C<K#G, V#G>>self.<eIU>map\MMap#T<§K#G§, §V#G§>\.<rdIa>iterator\Iterator#T<^(§K#G§, §V#G§)>\;
    }
    public MIterator<Tuple2<K, V>> mutableIterator() {
        return ERROR: Unknown <MMapDefault#C<K#G, V#G>>self.<eIU>map\MMap#T<§K#G§, §V#G§>\.<rdIa>mutableIterator\MIterator#T<^(§K#G§, §V#G§)>\;
    }
    public V applyKey(K key) {
        return ERROR: Unknown <MMapDefault#C<K#G, V#G>>self.<eIU>map\MMap#T<§K#G§, §V#G§>\.<dI>objectFor(key = <l>key\§K#G§\, orUpdateWith =  -> §V#G§ = return <MMapDefault#C<K#G, V#G>>self.<eIU>defaultFunc\§K#G§ -> §V#G§\.<d>apply( = <l>key\§K#G§\)\§V#G§\)\§V#G§\;
    }
    public Iterable<K> keys() {
        return ERROR: Unknown <MMapDefault#C<K#G, V#G>>self.<eIU>map\MMap#T<§K#G§, §V#G§>\.<rdIa>keys\Iterable#T<§K#G§>\;
    }
    public Iterable<V> values() {
        return ERROR: Unknown <MMapDefault#C<K#G, V#G>>self.<eIU>map\MMap#T<§K#G§, §V#G§>\.<rdIa>values\Iterable#T<§V#G§>\;
    }
    public boolean containsKey(K key) {
        return ERROR: Unknown <MMapDefault#C<K#G, V#G>>self.<eIU>map\MMap#T<§K#G§, §V#G§>\.<rdI>contains(key = <l>key\§K#G§\)\bool\;
    }
    public void setKeyValue(K key,V value) {
        ERROR: Unknown <MMapDefault#C<K#G, V#G>>self.<eIU>map\MMap#T<§K#G§, §V#G§>\.<dIa>set(key = <l>key\§K#G§\, value = <l>value\§V#G§\)\void\;
    }
    public V modifyKeyBy(K key,F<V, V> by) {
        ERROR: Unknown local value : V#G = <l>by\§V#G§ -> §V#G§\.<d>apply( = <MMapDefault#C<K#G, V#G>>self.<dI>apply(key = <l>key\§K#G§\)\§V#G§\)\§V#G§\;
        ERROR: Unknown <MMapDefault#C<K#G, V#G>>self.<eIU>map\MMap#T<§K#G§, §V#G§>\.<dIa>set(key = <l>key\§K#G§\, value = <l>value\§V#G§\)\void\;
        return ERROR: Unknown <l>value\§V#G§\;
    }
    public void appendItem(Tuple2<K, V> item) {
        ERROR: Unknown <MMapDefault#C<K#G, V#G>>self.<eIU>map\MMap#T<§K#G§, §V#G§>\.<dIo>append(item = <l>item\^(§K#G§, §V#G§)\)\void\;
    }
    public boolean removeItem(Tuple2<K, V> item) {
        return ERROR: Unknown <MMapDefault#C<K#G, V#G>>self.<eIU>map\MMap#T<§K#G§, §V#G§>\.<dIo>remove(item = <l>item\^(§K#G§, §V#G§)\)\bool\;
    }
    public void clear() {
        ERROR: Unknown <MMapDefault#C<K#G, V#G>>self.<eIU>map\MMap#T<§K#G§, §V#G§>\.<rdIa>clear\void\;
    }
    public ImMapDefault<K, V> im() {
        return new ImMapDefault(ERROR: Unknown <MMapDefault#C<K#G, V#G>>self.<eIU>map\MMap#T<§K#G§, §V#G§>\.<dIo>im\ImMap#T<§K#G§, §V#G§>\, ERROR: Unknown <MMapDefault#C<K#G, V#G>>self.<eIU>defaultFunc\§K#G§ -> §V#G§\);
    }
    public ImMapDefault<K, V> imCopy() {
        return new ImMapDefault(ERROR: Unknown <MMapDefault#C<K#G, V#G>>self.<eIU>map\MMap#T<§K#G§, §V#G§>\.<dIo>imCopy\ImMap#T<§K#G§, §V#G§>\, ERROR: Unknown <MMapDefault#C<K#G, V#G>>self.<eIU>defaultFunc\§K#G§ -> §V#G§\);
    }
    public MMapDefault(MMap<K, V> map,F<K, V> defaultFunc) {
    }
    static final ClassType<MMapDefault<K, V>> type;
    public void mutableFilterBy(F<T, Boolean> by) {
        ERROR: Unknown local i : MIterator#T<§T#G§> = <MIterable#T<T#G>>self.<dIa>mutableIterator\MIterator#T<§T#G§>\;
        ERROR: Unknown while(<l>i\MIterator#T<§T#G§>\.<rdIa>hasNext\bool\) {
    if(<l>by\§T#G§ -> bool\.<d>apply( = <l>i\MIterator#T<§T#G§>\.<rdIa>next\§T#G§\)\bool\) <l>i\MIterator#T<§T#G§>\.<dIa>remove\void\
};
    }
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
        ERROR: Unknown <Traversable#T<T#G>>self.<dIa>go(on = item : §T#G§ -> bool = {
    <l>each\§T#G§ -> void\.<d>apply( = <l>item\§T#G§\)\void\
    return True
})\bool\;
    }
    public void parForEach(F<T, Void> each) {
        ERROR: Unknown <Traversable#T<T#G>>self.<dIa>go(on = item : §T#G§ -> bool = {
    <to>DispatchQueue\DispatchQueue#C.class\.<eIt>default\DispatchQueue#C\.<dI>async(f =  -> void = <l>each\§T#G§ -> void\.<d>apply( = <l>item\§T#G§\)\void\)\void\
    return True
})\bool\;
    }
    public Chain<T> chain() {
        return ERROR: Unknown <to>Chain\Chain#C.class\.<dItu>chainWith(collection = <Traversable#T<T#G>>self)\Chain#C<§T#G§>\;
    }
    public T findWhere(F<T, Boolean> where) {
        ERROR: Unknown local var ret : (T#G)? = none<T#G>;
        ERROR: Unknown <Traversable#T<T#G>>self.<dIa>go(on = x : §T#G§ -> bool = if(<l>where\§T#G§ -> bool\.<d>apply( = <l>x\§T#G§\)\bool\) {
    (<lm>ret\(§T#G§)?\ = some(<l>x\§T#G§\)\§(T#G)?§\)
    return False
}
else return True)\bool\;
        return ERROR: Unknown <lm>ret\(§T#G§)?\;
    }
    public boolean existsWhere(F<T, Boolean> where) {
        ERROR: Unknown local var ret : bool = False;
        ERROR: Unknown <Traversable#T<T#G>>self.<dIa>go(on = x : §T#G§ -> bool = if(<l>where\§T#G§ -> bool\.<d>apply( = <l>x\§T#G§\)\bool\) {
    (<lm>ret\bool\ = True)
    return False
}
else return True)\bool\;
        return ERROR: Unknown <lm>ret\bool\;
    }
    public boolean allConfirm(F<T, Boolean> confirm) {
        ERROR: Unknown local var ret : bool = True;
        ERROR: Unknown <Traversable#T<T#G>>self.<dIa>go(on = x : §T#G§ -> bool = if(!(<l>confirm\§T#G§ -> bool\.<d>apply( = <l>x\§T#G§\)\bool\)) {
    (<lm>ret\bool\ = False)
    return False
}
else return True)\bool\;
        return ERROR: Unknown <lm>ret\bool\;
    }
    public T head() {
        ERROR: Unknown local var ret : (T#G)? = ;
        ERROR: Unknown <Traversable#T<T#G>>self.<dIa>go(on = on : §T#G§ -> bool = {
    (<lm>ret\(§T#G§)?\ = some(<l>on\§T#G§\)\§(T#G)?§\)
    return False
})\bool\;
        return ERROR: Unknown <lm>ret\(§T#G§)?\;
    }
    public C convertWithBuilder(Builder<T, C> builder) {
        ERROR: Unknown <Traversable#T<T#G>>self.<dI>for(each = x : §T#G§ -> void = <l>builder\Builder#T<§T#G§, C#G>\.<dIa>append(item = <l>x\§T#G§\)\void\)\void\;
        return ERROR: Unknown <l>builder\Builder#T<§T#G§, C#G>\.<dIa>build\§C#G§\;
    }
    public void forEach(F<T, Void> each) {
        ERROR: Unknown <Traversable#T<T#G>>self.<dIa>go(on = item : §T#G§ -> bool = {
    <l>each\§T#G§ -> void\.<d>apply( = <l>item\§T#G§\)\void\
    return True
})\bool\;
    }
    public void parForEach(F<T, Void> each) {
        ERROR: Unknown <Traversable#T<T#G>>self.<dIa>go(on = item : §T#G§ -> bool = {
    <to>DispatchQueue\DispatchQueue#C.class\.<eIt>default\DispatchQueue#C\.<dI>async(f =  -> void = <l>each\§T#G§ -> void\.<d>apply( = <l>item\§T#G§\)\void\)\void\
    return True
})\bool\;
    }
    public Chain<T> chain() {
        return ERROR: Unknown <to>Chain\Chain#C.class\.<dItu>chainWith(collection = <Traversable#T<T#G>>self)\Chain#C<§T#G§>\;
    }
    public T findWhere(F<T, Boolean> where) {
        ERROR: Unknown local var ret : (T#G)? = none<T#G>;
        ERROR: Unknown <Traversable#T<T#G>>self.<dIa>go(on = x : §T#G§ -> bool = if(<l>where\§T#G§ -> bool\.<d>apply( = <l>x\§T#G§\)\bool\) {
    (<lm>ret\(§T#G§)?\ = some(<l>x\§T#G§\)\§(T#G)?§\)
    return False
}
else return True)\bool\;
        return ERROR: Unknown <lm>ret\(§T#G§)?\;
    }
    public boolean existsWhere(F<T, Boolean> where) {
        ERROR: Unknown local var ret : bool = False;
        ERROR: Unknown <Traversable#T<T#G>>self.<dIa>go(on = x : §T#G§ -> bool = if(<l>where\§T#G§ -> bool\.<d>apply( = <l>x\§T#G§\)\bool\) {
    (<lm>ret\bool\ = True)
    return False
}
else return True)\bool\;
        return ERROR: Unknown <lm>ret\bool\;
    }
    public boolean allConfirm(F<T, Boolean> confirm) {
        ERROR: Unknown local var ret : bool = True;
        ERROR: Unknown <Traversable#T<T#G>>self.<dIa>go(on = x : §T#G§ -> bool = if(!(<l>confirm\§T#G§ -> bool\.<d>apply( = <l>x\§T#G§\)\bool\)) {
    (<lm>ret\bool\ = False)
    return False
}
else return True)\bool\;
        return ERROR: Unknown <lm>ret\bool\;
    }
    public T head() {
        ERROR: Unknown local var ret : (T#G)? = ;
        ERROR: Unknown <Traversable#T<T#G>>self.<dIa>go(on = on : §T#G§ -> bool = {
    (<lm>ret\(§T#G§)?\ = some(<l>on\§T#G§\)\§(T#G)?§\)
    return False
})\bool\;
        return ERROR: Unknown <lm>ret\(§T#G§)?\;
    }
    public C convertWithBuilder(Builder<T, C> builder) {
        ERROR: Unknown <Traversable#T<T#G>>self.<dI>for(each = x : §T#G§ -> void = <l>builder\Builder#T<§T#G§, C#G>\.<dIa>append(item = <l>x\§T#G§\)\void\)\void\;
        return ERROR: Unknown <l>builder\Builder#T<§T#G§, C#G>\.<dIa>build\§C#G§\;
    }
}