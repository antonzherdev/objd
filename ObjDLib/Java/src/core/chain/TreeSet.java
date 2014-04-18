package core.chain;

public class TreeSet<T> implements Set<T> {
    public final TreeMap<T, Object> map;
    public T higherThanItem(T item) {
        return ERROR: Unknown <TreeSet#C<T#G>>self.<eIU>map\TreeMap#C<§T#G§, ^Object#C>\.<dI>higherKeyThan(key = <l>item\§T#G§\)\(§T#G§)?\;
    }
    public T lowerThanItem(T item) {
        return ERROR: Unknown <TreeSet#C<T#G>>self.<eIU>map\TreeMap#C<§T#G§, ^Object#C>\.<dI>lowerKeyThan(key = <l>item\§T#G§\)\(§T#G§)?\;
    }
    public int count() {
        return ERROR: Unknown <TreeSet#C<T#G>>self.<eIU>map\TreeMap#C<§T#G§, ^Object#C>\.<rdI>count\uint\;
    }
    public Iterator<T> iterator() {
        return ERROR: Unknown <TreeSet#C<T#G>>self.<eIU>map\TreeMap#C<§T#G§, ^Object#C>\.<dIoa>keys\TreeMapKeySet#T<§T#G§>\.<rdIa>iterator\Iterator#T<§T#G§>\;
    }
    public Iterator<T> iteratorHigherThanItem(T item) {
        return ERROR: Unknown <TreeSet#C<T#G>>self.<eIU>map\TreeMap#C<§T#G§, ^Object#C>\.<dIoa>keys\TreeMapKeySet#T<§T#G§>\.<dIa>iteratorHigherThan(key = <l>item\§T#G§\)\Iterator#T<§T#G§>\;
    }
    public T head() {
        return ERROR: Unknown <TreeSet#C<T#G>>self.<eIU>map\TreeMap#C<§T#G§, ^Object#C>\.<dI>firstKey\(§T#G§)?\;
    }
    public T last() {
        return ERROR: Unknown <TreeSet#C<T#G>>self.<eIU>map\TreeMap#C<§T#G§, ^Object#C>\.<dI>lastKey\(§T#G§)?\;
    }
    public boolean containsItem(T item) {
        return ERROR: Unknown <TreeSet#C<T#G>>self.<eIU>map\TreeMap#C<§T#G§, ^Object#C>\.<rdI>contains(key = <l>item\§T#G§\)\bool\;
    }
    public TreeSet(TreeMap<T, Object> map) {
    }
    static final ClassType<TreeSet<T>> type;
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
    public C convertWithBuilder(Builder<T, C> builder) {
        ERROR: Unknown <Traversable#T<T#G>>self.<dI>for(each = x : §T#G§ -> void = <l>builder\Builder#T<§T#G§, C#G>\.<dIa>append(item = <l>x\§T#G§\)\void\)\void\;
        return ERROR: Unknown <l>builder\Builder#T<§T#G§, C#G>\.<dIa>build\§C#G§\;
    }
}