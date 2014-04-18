package core.chain;

public class TreeMap<K, V> implements ImMap<K, V> {
    public static final int BLACK = ERROR: Unknown 0;
    public static final int RED = ERROR: Unknown 1;
    public final F2<K, K, Integer> comparator;
    public final TreeMapValues<V> values = new TreeMapValues(ERROR: Unknown <TreeMap#C<K#G, V#G>>self);
    public V applyKey(K key) {
        return ERROR: Unknown <TreeMap#C<K#G, V#G>>self.<dI>entryFor(key = <l>key\§K#G§\)\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIUm>value\§V#G§\;
    }
    public V optKey(K key) {
        return ERROR: Unknown <TreeMap#C<K#G, V#G>>self.<dI>entryFor(key = <l>key\§K#G§\)\(^TreeMapEntry#C<§K#G§, §V#G§>)?\?.<eIUm>value\§V#G§\;
    }
    public abstract TreeMapEntry<K, V> root();
    public boolean isEmpty() {
        return ERROR: Unknown (<TreeMap#C<K#G, V#G>>self.<dIa>root\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == none<^TreeMapEntry#C<§K#G§, §V#G§>>);
    }
    public TreeMapEntry<K, V> entryForKey(K key) {
        ERROR: Unknown local var p : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <TreeMap#C<K#G, V#G>>self.<dIa>root\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
        ERROR: Unknown while((<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>)) {
    local cmp : int = <TreeMap#C<K#G, V#G>>self.<eIU>comparator\(§K#G§, §K#G§) -> int\.<d>apply( = <l>key\§K#G§\,  = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIUm>key\§K#G§\)\int\
    if((<l>cmp\int\ < 0)) (<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
else if((<l>cmp\int\ > 0)) (<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)
else break
};
        return ERROR: Unknown <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
    }
    public abstract TreeMapKeySet<K> keys();
    public Iterator<Tuple2<K, V>> iterator() {
        return ERROR: Unknown <to>TreeMapIterator\TreeMapIterator#C.class\.<dIt>apply(map = <TreeMap#C<K#G, V#G>>self, entry = <TreeMap#C<K#G, V#G>>self.<dI>firstEntry\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)\TreeMapIterator#C<§K#G§, §V#G§>\;
    }
    public TreeMapIterator<K, V> iteratorHigherThanKey(K key) {
        return ERROR: Unknown <to>TreeMapIterator\TreeMapIterator#C.class\.<dIt>apply(map = <TreeMap#C<K#G, V#G>>self, entry = <TreeMap#C<K#G, V#G>>self.<dp>higherEntryThan(key = <l>key\§K#G§\)\(^TreeMapEntry#C<§K#G§, §V#G§>)?\)\TreeMapIterator#C<§K#G§, §V#G§>\;
    }
    public TreeMapEntry<K, V> firstEntry() {
        ERROR: Unknown local var p : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <TreeMap#C<K#G, V#G>>self.<dIa>root\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
        ERROR: Unknown if((<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>)) while((<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>)) (<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>left\(^TreeMapEntry#C<§K#G§, §V#G§>)?\);
        return ERROR: Unknown <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
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
        return ERROR: Unknown none<^TreeMapEntry#C<K#G, V#G>>;
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
        return ERROR: Unknown none<^TreeMapEntry#C<K#G, V#G>>;
    }
    private TreeMapEntry<K, V> lastEntry() {
        ERROR: Unknown local var p : (^TreeMapEntry#C<§K#G§, §V#G§>)? = <TreeMap#C<K#G, V#G>>self.<dIa>root\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
        ERROR: Unknown if((<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>)) while((<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ != none<^TreeMapEntry#C<§K#G§, §V#G§>>)) (<lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\ = <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)¿\.<eIm>right\(^TreeMapEntry#C<§K#G§, §V#G§>)?\);
        return ERROR: Unknown <lm>p\(^TreeMapEntry#C<§K#G§, §V#G§>)?\;
    }
    public TreeMap(F2<K, K, Integer> comparator) {
    }
    static final ClassType<TreeMap<K, V>> type;
    public ImMap<K, V> addItem(Tuple2<K, V> item) {
        ERROR: Unknown local builder : HashMapBuilder#C<§K#G§, §V#G§> = <to>HashMapBuilder\HashMapBuilder#C.class\.<tcI>apply\HashMapBuilder#C<§K#G§, §V#G§>\;
        ERROR: Unknown <l>builder\HashMapBuilder#C<§K#G§, §V#G§>\.<rdI>appendAll(items = <ImMap#T<K#G, V#G>>self)\void\;
        ERROR: Unknown <l>builder\HashMapBuilder#C<§K#G§, §V#G§>\.<dIo>append(item = <l>item\(§K#G§, §V#G§)\)\void\;
        return ERROR: Unknown <l>builder\HashMapBuilder#C<§K#G§, §V#G§>\.<dIo>build\^ImHashMap#C<§K#G§, §V#G§>\;
    }
    public MMap<K, V> mCopy() {
        ERROR: Unknown local m : MHashMap#C<§K#G§, §V#G§> = <to>MHashMap\MHashMap#C.class\.<tcI>apply\MHashMap#C<§K#G§, §V#G§>\;
        ERROR: Unknown <l>m\MHashMap#C<§K#G§, §V#G§>\.<rdI>assign(imMap = <ImMap#T<K#G, V#G>>self)\void\;
        return ERROR: Unknown <l>m\MHashMap#C<§K#G§, §V#G§>\;
    }
    public V getKeyOrValue(K key,V orValue) {
        ERROR: Unknown local __tmp : §(V#G)?§ = <Map#T<K#G, V#G>>self.<dIa>opt(key = <l>key\§K#G§\)\(§V#G§)?\;
        ERROR: Unknown if((<l>__tmp\§(V#G)?§\ != none<§V#G§>)) return <l>__tmp\§(V#G)?§\.get
else return <l>orValue\§V#G§\;
    }
    public boolean containsKey(K key) {
        return ERROR: Unknown (<Map#T<K#G, V#G>>self.<dIa>opt(key = <l>key\§K#G§\)\(§V#G§)?\ != none<§V#G§>);
    }
    public boolean isValueEqualKeyValue(K key,V value) {
        ERROR: Unknown val __tmp : ^(^bool)?
{
    local _ : §(V#G)¿§ = <Map#T<K#G, V#G>>self.<dIa>opt(key = <l>key\§K#G§\)\(§V#G§)?\
    if((<l>_\§(V#G)¿§\ != none<§V#G§>)) (<l>__tmp\^(^bool)?\ = some((<l>_\§(V#G)¿§\ == <l>value\§V#G§\))\(^bool)?\)
else (<l>__tmp\^(^bool)?\ = none<^bool>)
};
        ERROR: Unknown if((<l>__tmp\^(^bool)?\ != none<^bool>)) return <l>__tmp\^(^bool)?\.get
else return False;
    }
    public int count() {
        ERROR: Unknown local i : Iterator#T<§T#G§> = <Iterable#T<T#G>>self.<dIa>iterator\Iterator#T<§T#G§>\;
        ERROR: Unknown local var n : uint = 0.cast<uint>;
        ERROR: Unknown while(<l>i\Iterator#T<§T#G§>\.<dIa>hasNext\bool\) {
    <l>i\Iterator#T<§T#G§>\.<dIa>next\§T#G§\
    <lm>n\uint\++
};
        return ERROR: Unknown <lm>n\uint\;
    }
    public T head() {
        ERROR: Unknown if(<Iterable#T<T#G>>self.<dI>isEmpty\bool\) return none<T#G>
else return some(<Iterable#T<T#G>>self.<dIa>iterator\Iterator#T<§T#G§>\.<dIa>next\§T#G§\)\§(T#G)?§\;
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
    public MIterable<T> mCopy() {
        ERROR: Unknown local arr : MArray#C<§T#G§> = <to>MArray\MArray#C.class\.<tcI>apply\MArray#C<§T#G§>\;
        ERROR: Unknown <ImIterable#T<T#G>>self.<rdIo>for(each = item : §T#G§ -> void = <l>arr\MArray#C<§T#G§>\.<rdIa>append(item = <l>item\§T#G§\)\void\)\void\;
        return ERROR: Unknown <l>arr\MArray#C<§T#G§>\;
    }
    public int count() {
        ERROR: Unknown local i : Iterator#T<§T#G§> = <Iterable#T<T#G>>self.<dIa>iterator\Iterator#T<§T#G§>\;
        ERROR: Unknown local var n : uint = 0.cast<uint>;
        ERROR: Unknown while(<l>i\Iterator#T<§T#G§>\.<dIa>hasNext\bool\) {
    <l>i\Iterator#T<§T#G§>\.<dIa>next\§T#G§\
    <lm>n\uint\++
};
        return ERROR: Unknown <lm>n\uint\;
    }
    public T head() {
        ERROR: Unknown if(<Iterable#T<T#G>>self.<dI>isEmpty\bool\) return none<T#G>
else return some(<Iterable#T<T#G>>self.<dIa>iterator\Iterator#T<§T#G§>\.<dIa>next\§T#G§\)\§(T#G)?§\;
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
    public MTraversable<T> mCopy() {
        ERROR: Unknown local arr : MArray#C<§T#G§> = <to>MArray\MArray#C.class\.<tcI>apply\MArray#C<§T#G§>\;
        ERROR: Unknown <ImTraversable#T<T#G>>self.<rdI>for(each = item : §T#G§ -> void = <l>arr\MArray#C<§T#G§>\.<rdIa>append(item = <l>item\§T#G§\)\void\)\void\;
        return ERROR: Unknown <l>arr\MArray#C<§T#G§>\;
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