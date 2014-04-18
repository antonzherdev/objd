package core.chain;

public class MList<T> implements MSeq<T> {
    private int _count = ERROR: Unknown 0.cast<uint>;
    private MListItem<T> headItem;
    private MListItem<T> lastItem;
    public int count() {
        return ERROR: Unknown <MList#C<T#G>>self.<emp>_count\uint\;
    }
    public Iterator<T> iterator() {
        ERROR: Unknown local i : MListImmutableIterator#C<§T#G§> = <to>MListImmutableIterator\MListImmutableIterator#C.class\.<tcI>apply\MListImmutableIterator#C<§T#G§>\;
        ERROR: Unknown (<l>i\MListImmutableIterator#C<§T#G§>\.<eImw>item\(^MListItem#C<§T#G§>)?\ = <MList#C<T#G>>self.<emp>headItem\(^MListItem#C<§T#G§>)?\);
        return ERROR: Unknown <l>i\MListImmutableIterator#C<§T#G§>\;
    }
    public MIterator<T> mutableIterator() {
        ERROR: Unknown local i : MListIterator#C<§T#G§> = <to>MListIterator\MListIterator#C.class\.<tcI>apply(list = <MList#C<T#G>>self)\MListIterator#C<§T#G§>\;
        ERROR: Unknown (<l>i\MListIterator#C<§T#G§>\.<eIm>item\(^MListItem#C<§T#G§>)?\ = <MList#C<T#G>>self.<emp>headItem\(^MListItem#C<§T#G§>)?\);
        return ERROR: Unknown <l>i\MListIterator#C<§T#G§>\;
    }
    public void insertIndexItem(int index,T item) {
        ERROR: Unknown if((<l>index\uint\ == 0)) <MList#C<T#G>>self.<dIo>prepend(item = <l>item\§T#G§\)\void\
else if((<l>index\uint\ >= <MList#C<T#G>>self.<emp>_count\uint\)) <MList#C<T#G>>self.<dIo>append(item = <l>item\§T#G§\)\void\
else {
    local var c : (^MListItem#C<§T#G§>)? = <MList#C<T#G>>self.<emp>headItem\(^MListItem#C<§T#G§>)?\
    local var i : uint = <l>index\uint\
    while(((<lm>c\(^MListItem#C<§T#G§>)?\ != none<^MListItem#C<§T#G§>>) && (<lm>i\uint\ > 0))) {
    (<lm>c\(^MListItem#C<§T#G§>)¿\ = <lm>c\(^MListItem#C<§T#G§>)¿\.<eIm>next\(^MListItem#C<§T#G§>)?\)
}
    if((<lm>c\(^MListItem#C<§T#G§>)?\ != none<^MListItem#C<§T#G§>>)) {
    local li : MListItem#C<§T#G§> = <to>MListItem\MListItem#C.class\.<tcI>apply(data = <l>item\§T#G§\)\MListItem#C<§T#G§>\
    {
    local __tmp_0_3_1 : (^MListItem#C<§T#G§>)? = <lm>c\(^MListItem#C<§T#G§>)¿\.<eIm>next\(^MListItem#C<§T#G§>)?\
    if((<l>__tmp_0_3_1\(^MListItem#C<§T#G§>)?\ != none<(^MListItem#C<§T#G§>)?>)) (<l>__tmp_0_3_1\(^MListItem#C<§T#G§>)?\.get.<eImw>prev\(^MListItem#C<§T#G§>)?\ = some(<l>li\MListItem#C<§T#G§>\)\(^MListItem#C<§T#G§>)?\)
}
    (<lm>c\(^MListItem#C<§T#G§>)¿\.<eIm>next\(^MListItem#C<§T#G§>)?\ = some(<l>li\MListItem#C<§T#G§>\)\(^MListItem#C<§T#G§>)?\)
}
else {
    <MList#C<T#G>>self.<dIo>append(item = <l>item\§T#G§\)\void\
}
};
    }
    public void prependItem(T item) {
        ERROR: Unknown local i : MListItem#C<§T#G§> = <to>MListItem\MListItem#C.class\.<tcI>apply(data = <l>item\§T#G§\)\MListItem#C<§T#G§>\;
        ERROR: Unknown if((<MList#C<T#G>>self.<emp>headItem\(^MListItem#C<§T#G§>)?\ == none<^MListItem#C<§T#G§>>)) {
    (<MList#C<T#G>>self.<emp>headItem\(^MListItem#C<§T#G§>)?\ = some(<l>i\MListItem#C<§T#G§>\)\(^MListItem#C<§T#G§>)?\)
    (<MList#C<T#G>>self.<emp>lastItem\(^MListItem#C<§T#G§>)?\ = some(<l>i\MListItem#C<§T#G§>\)\(^MListItem#C<§T#G§>)?\)
    (<MList#C<T#G>>self.<emp>_count\uint\ = 1.cast<uint>)
}
else {
    (<l>i\MListItem#C<§T#G§>\.<eIm>next\(^MListItem#C<§T#G§>)?\ = <MList#C<T#G>>self.<emp>headItem\(^MListItem#C<§T#G§>)¿\.cast<(^MListItem#C<§T#G§>)?>)
    (<MList#C<T#G>>self.<emp>headItem\(^MListItem#C<§T#G§>)¿\.<eImw>prev\(^MListItem#C<§T#G§>)?\ = some(<l>i\MListItem#C<§T#G§>\)\(^MListItem#C<§T#G§>)?\)
    (<MList#C<T#G>>self.<emp>headItem\(^MListItem#C<§T#G§>)¿\ = some(<l>i\MListItem#C<§T#G§>\)\(^MListItem#C<§T#G§>)¿\)
    <MList#C<T#G>>self.<emp>_count\uint\++
};
    }
    public void appendItem(T item) {
        ERROR: Unknown local i : MListItem#C<§T#G§> = <to>MListItem\MListItem#C.class\.<tcI>apply(data = <l>item\§T#G§\)\MListItem#C<§T#G§>\;
        ERROR: Unknown if((<MList#C<T#G>>self.<emp>lastItem\(^MListItem#C<§T#G§>)?\ == none<^MListItem#C<§T#G§>>)) {
    (<MList#C<T#G>>self.<emp>headItem\(^MListItem#C<§T#G§>)?\ = some(<l>i\MListItem#C<§T#G§>\)\(^MListItem#C<§T#G§>)?\)
    (<MList#C<T#G>>self.<emp>lastItem\(^MListItem#C<§T#G§>)?\ = some(<l>i\MListItem#C<§T#G§>\)\(^MListItem#C<§T#G§>)?\)
    (<MList#C<T#G>>self.<emp>_count\uint\ = 1.cast<uint>)
}
else {
    (<l>i\MListItem#C<§T#G§>\.<eImw>prev\(^MListItem#C<§T#G§>)?\ = <MList#C<T#G>>self.<emp>lastItem\(^MListItem#C<§T#G§>)¿\.cast<(^MListItem#C<§T#G§>)?>)
    (<MList#C<T#G>>self.<emp>lastItem\(^MListItem#C<§T#G§>)¿\.<eIm>next\(^MListItem#C<§T#G§>)?\ = some(<l>i\MListItem#C<§T#G§>\)\(^MListItem#C<§T#G§>)?\)
    (<MList#C<T#G>>self.<emp>lastItem\(^MListItem#C<§T#G§>)¿\ = some(<l>i\MListItem#C<§T#G§>\)\(^MListItem#C<§T#G§>)¿\)
    <MList#C<T#G>>self.<emp>_count\uint\++
};
    }
    public void removeListItem(MListItem<T> listItem) {
        ERROR: Unknown if(((<MList#C<T#G>>self.<emp>headItem\(^MListItem#C<§T#G§>)?\ != none<^MListItem#C<§T#G§>>) && (<MList#C<T#G>>self.<emp>headItem\(^MListItem#C<§T#G§>)?\ == <l>listItem\MListItem#C<§T#G§>\))) {
    (<MList#C<T#G>>self.<emp>headItem\(^MListItem#C<§T#G§>)¿\ = <MList#C<T#G>>self.<emp>headItem\(^MListItem#C<§T#G§>)¿\.<eIm>next\(^MListItem#C<§T#G§>)?\)
    (<MList#C<T#G>>self.<emp>headItem\(^MListItem#C<§T#G§>)¿\.<eImw>prev\(^MListItem#C<§T#G§>)?\ = none<^MListItem#C<§T#G§>>)
}
else if(((<MList#C<T#G>>self.<emp>lastItem\(^MListItem#C<§T#G§>)?\ != none<^MListItem#C<§T#G§>>) && (<MList#C<T#G>>self.<emp>lastItem\(^MListItem#C<§T#G§>)?\ == <l>listItem\MListItem#C<§T#G§>\))) {
    (<MList#C<T#G>>self.<emp>lastItem\(^MListItem#C<§T#G§>)¿\ = <MList#C<T#G>>self.<emp>lastItem\(^MListItem#C<§T#G§>)¿\.<eImw>prev\(^MListItem#C<§T#G§>)?\)
    (<MList#C<T#G>>self.<emp>lastItem\(^MListItem#C<§T#G§>)¿\.<eIm>next\(^MListItem#C<§T#G§>)?\ = none<^MListItem#C<§T#G§>>)
}
else {
    {
    local __tmp_0_0 : (^MListItem#C<§T#G§>)? = <l>listItem\MListItem#C<§T#G§>\.<eImw>prev\(^MListItem#C<§T#G§>)?\
    if((<l>__tmp_0_0\(^MListItem#C<§T#G§>)?\ != none<(^MListItem#C<§T#G§>)?>)) (<l>__tmp_0_0\(^MListItem#C<§T#G§>)?\.get.<eIm>next\(^MListItem#C<§T#G§>)?\ = <l>listItem\MListItem#C<§T#G§>\.<eIm>next\(^MListItem#C<§T#G§>)?\)
}
    {
    local __tmp_0_1 : (^MListItem#C<§T#G§>)? = <l>listItem\MListItem#C<§T#G§>\.<eIm>next\(^MListItem#C<§T#G§>)?\
    if((<l>__tmp_0_1\(^MListItem#C<§T#G§>)?\ != none<(^MListItem#C<§T#G§>)?>)) (<l>__tmp_0_1\(^MListItem#C<§T#G§>)?\.get.<eImw>prev\(^MListItem#C<§T#G§>)?\ = <l>listItem\MListItem#C<§T#G§>\.<eImw>prev\(^MListItem#C<§T#G§>)?\)
}
};
        ERROR: Unknown <MList#C<T#G>>self.<emp>_count\uint\--;
    }
    public void clear() {
        ERROR: Unknown (<MList#C<T#G>>self.<emp>headItem\(^MListItem#C<§T#G§>)?\ = none<^MListItem#C<§T#G§>>);
        ERROR: Unknown (<MList#C<T#G>>self.<emp>lastItem\(^MListItem#C<§T#G§>)?\ = none<^MListItem#C<§T#G§>>);
    }
    public void removeHead() {
        {
            ERROR: Unknown local _ : ^(^MListItem#C<§T#G§>)¿ = <MList#C<T#G>>self.<emp>headItem\(^MListItem#C<§T#G§>)?\;
            ERROR: Unknown if((<l>_\^(^MListItem#C<§T#G§>)¿\ != none<^MListItem#C<§T#G§>>)) <MList#C<T#G>>self.<dI>remove(listItem = <l>_\^(^MListItem#C<§T#G§>)¿\)\void\;
        }
    }
    public void removeLast() {
        {
            ERROR: Unknown local _ : ^(^MListItem#C<§T#G§>)¿ = <MList#C<T#G>>self.<emp>lastItem\(^MListItem#C<§T#G§>)?\;
            ERROR: Unknown if((<l>_\^(^MListItem#C<§T#G§>)¿\ != none<^MListItem#C<§T#G§>>)) <MList#C<T#G>>self.<dI>remove(listItem = <l>_\^(^MListItem#C<§T#G§>)¿\)\void\;
        }
    }
    public T takeHead() {
        ERROR: Unknown local h : ^(^MListItem#C<§T#G§>)¿ = <MList#C<T#G>>self.<emp>headItem\(^MListItem#C<§T#G§>)?\;
        ERROR: Unknown if((<l>h\^(^MListItem#C<§T#G§>)¿\ != none<^MListItem#C<§T#G§>>)) {
    local r : T#G = <l>h\^(^MListItem#C<§T#G§>)¿\.<eIUm>data\§T#G§\
    <MList#C<T#G>>self.<dI>remove(listItem = <l>h\^(^MListItem#C<§T#G§>)¿\)\void\
    return some(<l>r\§T#G§\)\§(T#G)?§\
}
else return none<§T#G§>;
    }
    public T last() {
        return ERROR: Unknown <MList#C<T#G>>self.<emp>lastItem\(^MListItem#C<§T#G§>)?\?.<eIUm>data\§T#G§\;
    }
    public T takeLast() {
        ERROR: Unknown local h : ^(^MListItem#C<§T#G§>)¿ = <MList#C<T#G>>self.<emp>lastItem\(^MListItem#C<§T#G§>)?\;
        ERROR: Unknown if((<l>h\^(^MListItem#C<§T#G§>)¿\ != none<^MListItem#C<§T#G§>>)) {
    local r : T#G = <l>h\^(^MListItem#C<§T#G§>)¿\.<eIUm>data\§T#G§\
    <MList#C<T#G>>self.<dI>remove(listItem = <l>h\^(^MListItem#C<§T#G§>)¿\)\void\
    return some(<l>r\§T#G§\)\§(T#G)?§\
}
else return none<§T#G§>;
    }
    public void forEach(F<T, Void> each) {
        ERROR: Unknown local var i : (^MListItem#C<§T#G§>)? = <MList#C<T#G>>self.<emp>headItem\(^MListItem#C<§T#G§>)?\;
        ERROR: Unknown while((<lm>i\(^MListItem#C<§T#G§>)?\ != none<^MListItem#C<§T#G§>>)) {
    <l>each\§T#G§ -> void\.<d>apply( = <lm>i\(^MListItem#C<§T#G§>)¿\.<eIUm>data\§T#G§\)\void\
    (<lm>i\(^MListItem#C<§T#G§>)¿\ = <lm>i\(^MListItem#C<§T#G§>)¿\.<eIm>next\(^MListItem#C<§T#G§>)?\)
};
    }
    public boolean goOn(F<T, Boolean> on) {
        ERROR: Unknown local var i : (^MListItem#C<§T#G§>)? = <MList#C<T#G>>self.<emp>headItem\(^MListItem#C<§T#G§>)?\;
        ERROR: Unknown while((<lm>i\(^MListItem#C<§T#G§>)?\ != none<^MListItem#C<§T#G§>>)) {
    if(!(<l>on\§T#G§ -> bool\.<d>apply( = <lm>i\(^MListItem#C<§T#G§>)¿\.<eIUm>data\§T#G§\)\bool\)) return False
    (<lm>i\(^MListItem#C<§T#G§>)¿\ = <lm>i\(^MListItem#C<§T#G§>)¿\.<eIm>next\(^MListItem#C<§T#G§>)?\)
};
        return ERROR: Unknown True;
    }
    public void mutableFilterBy(F<T, Boolean> by) {
        ERROR: Unknown local var i : (^MListItem#C<§T#G§>)? = <MList#C<T#G>>self.<emp>headItem\(^MListItem#C<§T#G§>)?\;
        ERROR: Unknown while((<lm>i\(^MListItem#C<§T#G§>)?\ != none<^MListItem#C<§T#G§>>)) {
    if(!(<l>by\§T#G§ -> bool\.<d>apply( = <lm>i\(^MListItem#C<§T#G§>)¿\.<eIUm>data\§T#G§\)\bool\)) <MList#C<T#G>>self.<dI>remove(listItem = <lm>i\(^MListItem#C<§T#G§>)¿\)\void\
    (<lm>i\(^MListItem#C<§T#G§>)¿\ = <lm>i\(^MListItem#C<§T#G§>)¿\.<eIm>next\(^MListItem#C<§T#G§>)?\)
};
    }
    public T head() {
        return ERROR: Unknown <MList#C<T#G>>self.<emp>headItem\(^MListItem#C<§T#G§>)?\?.<eIUm>data\§T#G§\;
    }
    public MList() {
    }
    static final ClassType<MList<T>> type;
    public boolean removeIndex(int index) {
        ERROR: Unknown local i : MIterator#T<§T#G§> = <MSeq#T<T#G>>self.<rdIa>mutableIterator\MIterator#T<§T#G§>\;
        ERROR: Unknown local var j : uint = <l>index\uint\;
        ERROR: Unknown local var ret : bool = False;
        ERROR: Unknown while(<l>i\MIterator#T<§T#G§>\.<rdIa>hasNext\bool\) {
    <l>i\MIterator#T<§T#G§>\.<rdIa>next\§T#G§\
    if((<lm>j\uint\ == 0)) {
    <l>i\MIterator#T<§T#G§>\.<dIa>remove\void\
    (<lm>ret\bool\ = True)
    break
}
    <lm>j\uint\--
};
        return ERROR: Unknown <lm>ret\bool\;
    }
    public void setIndexItem(int index,T item) {
        ERROR: Unknown local i : MIterator#T<§T#G§> = <MSeq#T<T#G>>self.<rdIa>mutableIterator\MIterator#T<§T#G§>\;
        ERROR: Unknown local var n : uint = <l>index\uint\;
        ERROR: Unknown while(<l>i\MIterator#T<§T#G§>\.<rdIa>hasNext\bool\) {
    if((<lm>n\uint\ == 0)) {
    <l>i\MIterator#T<§T#G§>\.<rdIa>next\§T#G§\
    <l>i\MIterator#T<§T#G§>\.<dIa>set(value = <l>item\§T#G§\)\void\
    return nil
}
    <l>i\MIterator#T<§T#G§>\.<rdIa>next\§T#G§\
    <lm>n\uint\--
};
        ERROR: Unknown throw "Incorrect index";
    }
    public ImSeq<T> im() {
        return ERROR: Unknown <MSeq#T<T#G>>self.<dIo>imCopy\ImSeq#T<§T#G§>\;
    }
    public ImSeq<T> imCopy() {
        ERROR: Unknown local arr : MArray#C<§T#G§> = <to>MArray\MArray#C.class\.<tcI>apply\MArray#C<§T#G§>\;
        ERROR: Unknown <MSeq#T<T#G>>self.<rdIo>for(each = item : §T#G§ -> void = <l>arr\MArray#C<§T#G§>\.<rdIa>append(item = <l>item\§T#G§\)\void\)\void\;
        return ERROR: Unknown <l>arr\MArray#C<§T#G§>\.<dIo>im\[§T#G§]\;
    }
    public T applyIndex(int index) {
        ERROR: Unknown if((<l>index\uint\ >= <Seq#T<T#G>>self.<rdI>count\uint\)) return none<T#G>;
        ERROR: Unknown local i : Iterator#T<§T#G§> = <Seq#T<T#G>>self.<rdIa>iterator\Iterator#T<§T#G§>\;
        ERROR: Unknown local var n : uint = <l>index\uint\;
        ERROR: Unknown while(<l>i\Iterator#T<§T#G§>\.<dIa>hasNext\bool\) {
    if((<lm>n\uint\ == 0)) return <l>i\Iterator#T<§T#G§>\.<dIa>next\§T#G§\
    <l>i\Iterator#T<§T#G§>\.<dIa>next\§T#G§\
    <lm>n\uint\--
};
        return ERROR: Unknown none<T#G>;
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
    public boolean isEmpty() {
        return ERROR: Unknown !(<Iterable#T<T#G>>self.<dIa>iterator\Iterator#T<§T#G§>\.<dIa>hasNext\bool\);
    }
    public void parForEach(F<T, Void> each) {
        ERROR: Unknown local i : Iterator#T<§T#G§> = <Iterable#T<T#G>>self.<dIa>iterator\Iterator#T<§T#G§>\;
        ERROR: Unknown while(<l>i\Iterator#T<§T#G§>\.<dIa>hasNext\bool\) {
    local v : T#G = <l>i\Iterator#T<§T#G§>\.<dIa>next\§T#G§\
    <to>DispatchQueue\DispatchQueue#C.class\.<eIt>default\DispatchQueue#C\.<dI>async(f =  -> void = <l>each\§T#G§ -> void\.<d>apply( = <l>v\§T#G§\)\void\)\void\
};
    }
    public boolean containsItem(T item) {
        ERROR: Unknown local i : Iterator#T<§T#G§> = <Iterable#T<T#G>>self.<dIa>iterator\Iterator#T<§T#G§>\;
        ERROR: Unknown while(<l>i\Iterator#T<§T#G§>\.<dIa>hasNext\bool\) if((<l>i\Iterator#T<§T#G§>\.<dIa>next\§T#G§\ == <l>i\Iterator#T<§T#G§>\)) return True;
        return ERROR: Unknown False;
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
    public boolean removeItem(T item) {
        ERROR: Unknown local i : MIterator#T<§T#G§> = <MIterable#T<T#G>>self.<dIa>mutableIterator\MIterator#T<§T#G§>\;
        ERROR: Unknown local var ret : bool = False;
        ERROR: Unknown while(<l>i\MIterator#T<§T#G§>\.<rdIa>hasNext\bool\) {
    if((<l>i\MIterator#T<§T#G§>\.<rdIa>next\§T#G§\ == <l>item\§T#G§\)) {
    <l>i\MIterator#T<§T#G§>\.<dIa>remove\void\
    (<lm>ret\bool\ = True)
}
};
        return ERROR: Unknown <lm>ret\bool\;
    }
    public ImIterable<T> im() {
        return ERROR: Unknown <MIterable#T<T#G>>self.<dIo>imCopy\ImIterable#T<§T#G§>\;
    }
    public ImIterable<T> imCopy() {
        ERROR: Unknown local arr : MArray#C<§T#G§> = <to>MArray\MArray#C.class\.<tcI>apply\MArray#C<§T#G§>\;
        ERROR: Unknown <MIterable#T<T#G>>self.<rdIo>for(each = item : §T#G§ -> void = <l>arr\MArray#C<§T#G§>\.<rdIa>append(item = <l>item\§T#G§\)\void\)\void\;
        return ERROR: Unknown <l>arr\MArray#C<§T#G§>\.<dIo>im\[§T#G§]\;
    }
    public boolean isEmpty() {
        return ERROR: Unknown !(<Iterable#T<T#G>>self.<dIa>iterator\Iterator#T<§T#G§>\.<dIa>hasNext\bool\);
    }
    public void parForEach(F<T, Void> each) {
        ERROR: Unknown local i : Iterator#T<§T#G§> = <Iterable#T<T#G>>self.<dIa>iterator\Iterator#T<§T#G§>\;
        ERROR: Unknown while(<l>i\Iterator#T<§T#G§>\.<dIa>hasNext\bool\) {
    local v : T#G = <l>i\Iterator#T<§T#G§>\.<dIa>next\§T#G§\
    <to>DispatchQueue\DispatchQueue#C.class\.<eIt>default\DispatchQueue#C\.<dI>async(f =  -> void = <l>each\§T#G§ -> void\.<d>apply( = <l>v\§T#G§\)\void\)\void\
};
    }
    public boolean containsItem(T item) {
        ERROR: Unknown local i : Iterator#T<§T#G§> = <Iterable#T<T#G>>self.<dIa>iterator\Iterator#T<§T#G§>\;
        ERROR: Unknown while(<l>i\Iterator#T<§T#G§>\.<dIa>hasNext\bool\) if((<l>i\Iterator#T<§T#G§>\.<dIa>next\§T#G§\ == <l>i\Iterator#T<§T#G§>\)) return True;
        return ERROR: Unknown False;
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
    public ImTraversable<T> im() {
        return ERROR: Unknown <MTraversable#T<T#G>>self.<dI>imCopy\ImTraversable#T<§T#G§>\;
    }
    public ImTraversable<T> imCopy() {
        ERROR: Unknown local arr : MArray#C<§T#G§> = <to>MArray\MArray#C.class\.<tcI>apply\MArray#C<§T#G§>\;
        ERROR: Unknown <MTraversable#T<T#G>>self.<rdI>for(each = item : §T#G§ -> void = <l>arr\MArray#C<§T#G§>\.<rdIa>append(item = <l>item\§T#G§\)\void\)\void\;
        return ERROR: Unknown <l>arr\MArray#C<§T#G§>\.<dIo>im\[§T#G§]\;
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