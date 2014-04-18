package core.chain;

public class IndexFunSeq<T> implements ImSeq<T> {
    public final int count;
    public final F<Integer, T> f;
    public T applyIndex(int index) {
        ERROR: Unknown if((<l>index\uint\ >= <IndexFunSeq#C<T#G>>self.<eIUo>count\uint\)) return none<T#G>
else return some(<IndexFunSeq#C<T#G>>self.<eIU>f\uint -> §T#G§\.<d>apply( = <l>index\uint\)\§T#G§\)\§(T#G)?§\;
    }
    public Iterator<T> iterator() {
        return new IndexFunSeqIterator(ERROR: Unknown <IndexFunSeq#C<T#G>>self.<eIUo>count\uint\, ERROR: Unknown <IndexFunSeq#C<T#G>>self.<eIU>f\uint -> §T#G§\);
    }
    public IndexFunSeq(int count,F<Integer, T> f) {
    }
    static final ClassType<IndexFunSeq<T>> type;
    public ImSeq<T> addItem(T item) {
        ERROR: Unknown local builder : ArrayBuilder#C<§T#G§> = <to>ArrayBuilder\ArrayBuilder#C.class\.<tcI>apply\ArrayBuilder#C<§T#G§>\;
        ERROR: Unknown <l>builder\ArrayBuilder#C<§T#G§>\.<rdI>appendAll(items = <ImSeq#T<T#G>>self)\void\;
        ERROR: Unknown <l>builder\ArrayBuilder#C<§T#G§>\.<dIo>append(item = <l>item\§T#G§\)\void\;
        return ERROR: Unknown <l>builder\ArrayBuilder#C<§T#G§>\.<dIo>build\^ImArray#C<§T#G§>\;
    }
    public ImSeq<T> addSeq(Seq<T> seq) {
        ERROR: Unknown local builder : ArrayBuilder#C<§T#G§> = <to>ArrayBuilder\ArrayBuilder#C.class\.<tcI>apply\ArrayBuilder#C<§T#G§>\;
        ERROR: Unknown <l>builder\ArrayBuilder#C<§T#G§>\.<rdI>appendAll(items = <ImSeq#T<T#G>>self)\void\;
        ERROR: Unknown <l>builder\ArrayBuilder#C<§T#G§>\.<rdI>appendAll(items = <l>seq\Seq#T<§T#G§>\)\void\;
        return ERROR: Unknown <l>builder\ArrayBuilder#C<§T#G§>\.<dIo>build\^ImArray#C<§T#G§>\;
    }
    public ImSeq<T> subItem(T item) {
        return ERROR: Unknown <ImSeq#T<T#G>>self.<rdI>chain\Chain#C<§T#G§>\.<dIu>filter( = _ : §T#G§ -> bool = return (<l>_\§T#G§\ != <l>item\§T#G§\))\Chain#C<§T#G§>\.<dIu>toArray\[§T#G§]\;
    }
    public MSeq<T> mCopy() {
        ERROR: Unknown local arr : MArray#C<§T#G§> = <to>MArray\MArray#C.class\.<tcI>apply\MArray#C<§T#G§>\;
        ERROR: Unknown <ImSeq#T<T#G>>self.<rdIo>for(each = item : §T#G§ -> void = <l>arr\MArray#C<§T#G§>\.<rdIa>append(item = <l>item\§T#G§\)\void\)\void\;
        return ERROR: Unknown <l>arr\MArray#C<§T#G§>\;
    }
    public Set<T> toSet() {
        return ERROR: Unknown <Seq#T<T#G>>self.<rdI>convertWith(builder = <to>HashSetBuilder\HashSetBuilder#C.class\.<tcI>apply\HashSetBuilder#C<§T#G§>\)\§^Set#T<T#G>§\;
    }
    public boolean isEqualSeq(Seq<T> seq) {
        ERROR: Unknown if((<Seq#T<T#G>>self.<rdI>count\uint\ != <l>seq\Seq#T<§T#G§>\.<rdI>count\uint\)) return False;
        ERROR: Unknown local ia : Iterator#T<§T#G§> = <Seq#T<T#G>>self.<rdIa>iterator\Iterator#T<§T#G§>\;
        ERROR: Unknown local ib : Iterator#T<§T#G§> = <l>seq\Seq#T<§T#G§>\.<rdIa>iterator\Iterator#T<§T#G§>\;
        ERROR: Unknown while((<l>ia\Iterator#T<§T#G§>\.<dIa>hasNext\bool\ && <l>ib\Iterator#T<§T#G§>\.<dIa>hasNext\bool\)) {
    if((<l>ia\Iterator#T<§T#G§>\.<dIa>next\§T#G§\ != <l>ib\Iterator#T<§T#G§>\.<dIa>next\§T#G§\)) return False
};
        return ERROR: Unknown True;
    }
    public boolean isEmpty() {
        return ERROR: Unknown (<Seq#T<T#G>>self.<rdI>count\uint\ == 0);
    }
    public T head() {
        return ERROR: Unknown <Seq#T<T#G>>self.<dI>apply(index = 0.cast<uint>)\(§T#G§)?\;
    }
    public T last() {
        return ERROR: Unknown <Seq#T<T#G>>self.<dI>apply(index = (<Seq#T<T#G>>self.<rdI>count\uint\ - 1))\(§T#G§)?\;
    }
    public ImSeq<T> tail() {
        ERROR: Unknown local builder : ArrayBuilder#C<§T#G§> = <to>ArrayBuilder\ArrayBuilder#C.class\.<tcI>apply\ArrayBuilder#C<§T#G§>\;
        ERROR: Unknown local i : Iterator#T<§T#G§> = <Seq#T<T#G>>self.<rdIa>iterator\Iterator#T<§T#G§>\;
        ERROR: Unknown if(<l>i\Iterator#T<§T#G§>\.<dIa>hasNext\bool\) {
    <l>i\Iterator#T<§T#G§>\.<dIa>next\§T#G§\
    while(<l>i\Iterator#T<§T#G§>\.<dIa>hasNext\bool\) {
    <l>builder\ArrayBuilder#C<§T#G§>\.<dIo>append(item = <l>i\Iterator#T<§T#G§>\.<dIa>next\§T#G§\)\void\
}
};
        return ERROR: Unknown <l>builder\ArrayBuilder#C<§T#G§>\.<dIo>build\^ImArray#C<§T#G§>\;
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
    public MIterable<T> mCopy() {
        ERROR: Unknown local arr : MArray#C<§T#G§> = <to>MArray\MArray#C.class\.<tcI>apply\MArray#C<§T#G§>\;
        ERROR: Unknown <ImIterable#T<T#G>>self.<rdIo>for(each = item : §T#G§ -> void = <l>arr\MArray#C<§T#G§>\.<rdIa>append(item = <l>item\§T#G§\)\void\)\void\;
        return ERROR: Unknown <l>arr\MArray#C<§T#G§>\;
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