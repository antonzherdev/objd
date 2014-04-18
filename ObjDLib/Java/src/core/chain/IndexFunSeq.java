package core.chain;

public class IndexFunSeq<T> implements ImSeq<T> {
    public final int count;
    public final F<Integer, T> f;
    public T applyIndex(int index) {
        ERROR: Unknown if((<l>index\uint\ >= <IndexFunSeq#C<T#G>>self.<eIUo>count\uint\)) return none<T#G>
else return some(<IndexFunSeq#C<T#G>>self.<eIU>f\uint -> §T#G§\.<d>apply( = <l>index\uint\)\§T#G§\)\§(T#G)?§\;
    }
    public Iterator<T> iterator() {
        return new IndexFunSeqIterator<T>(count, f);
    }
    public IndexFunSeq(int count,F<Integer, T> f) {
    }
    static final ClassType<IndexFunSeq<T>> type;
    public ImSeq<T> addItem(T item) {
        ERROR: Unknown local builder : ArrayBuilder#C<§T#G§> = <to>ArrayBuilder\ArrayBuilder#C.class\.<tcI>apply\ArrayBuilder#C<§T#G§>\;
        builder.appendAllItems(ERROR: Unknown <ImSeq#T<T#G>>self);
        builder.appendItem(item);
        return builder.build();
    }
    public ImSeq<T> addSeq(Seq<T> seq) {
        ERROR: Unknown local builder : ArrayBuilder#C<§T#G§> = <to>ArrayBuilder\ArrayBuilder#C.class\.<tcI>apply\ArrayBuilder#C<§T#G§>\;
        builder.appendAllItems(ERROR: Unknown <ImSeq#T<T#G>>self);
        builder.appendAllItems(seq);
        return builder.build();
    }
    public ImSeq<T> subItem(T item) {
        return chain().filter(ERROR: Unknown _ : §T#G§ -> bool = return (<l>_\§T#G§\ != <l>item\§T#G§\)).toArray();
    }
    public MSeq<T> mCopy() {
        ERROR: Unknown local arr : MArray#C<§T#G§> = <to>MArray\MArray#C.class\.<tcI>apply\MArray#C<§T#G§>\;
        forEach(ERROR: Unknown item : §T#G§ -> void = <l>arr\MArray#C<§T#G§>\.<rdIa>append(item = <l>item\§T#G§\)\void\);
        return arr;
    }
    public Set<T> toSet() {
        return convertWithBuilder<Set<T>>(new HashSetBuilder<T>());
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
        return applyIndex(ERROR: Unknown 0.cast<uint>);
    }
    public T last() {
        return applyIndex(ERROR: Unknown (<Seq#T<T#G>>self.<rdI>count\uint\ - 1));
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
        return builder.build();
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
    public MIterable<T> mCopy() {
        ERROR: Unknown local arr : MArray#C<§T#G§> = <to>MArray\MArray#C.class\.<tcI>apply\MArray#C<§T#G§>\;
        forEach(ERROR: Unknown item : §T#G§ -> void = <l>arr\MArray#C<§T#G§>\.<rdIa>append(item = <l>item\§T#G§\)\void\);
        return arr;
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
    public MTraversable<T> mCopy() {
        ERROR: Unknown local arr : MArray#C<§T#G§> = <to>MArray\MArray#C.class\.<tcI>apply\MArray#C<§T#G§>\;
        forEach(ERROR: Unknown item : §T#G§ -> void = <l>arr\MArray#C<§T#G§>\.<rdIa>append(item = <l>item\§T#G§\)\void\);
        return arr;
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