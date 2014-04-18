package core.chain;

public class MList<T> implements MSeq<T> {
    private int _count = ERROR: Unknown 0.cast<uint>;
    private MListItem<T> headItem;
    private MListItem<T> lastItem;
    @Override
    public int count() {
        return this._count;
    }
    @Override
    public Iterator<T> iterator() {
        ERROR: Unknown local i : MListImmutableIterator#C<§T#G§> = <to>MListImmutableIterator\MListImmutableIterator#C.class\.<tcI>apply\MListImmutableIterator#C<§T#G§>\;
        i.item = this.headItem;
        return i;
    }
    @Override
    public MIterator<T> mutableIterator() {
        ERROR: Unknown local i : MListIterator#C<§T#G§> = <to>MListIterator\MListIterator#C.class\.<tcI>apply(list = <MList#C<T#G>>self)\MListIterator#C<§T#G§>\;
        i.item = this.headItem;
        return i;
    }
    @Override
    public void insertIndexItem(int index,T item) {
        if(index.equals(ERROR: Unknown 0)) {
            prependItem(item);
        } else {
            if(index >= this._count) {
                appendItem(item);
            } else {
                ERROR: Unknown local var c : (^MListItem#C<§T#G§>)? = <MList#C<T#G>>self.<emp>headItem\(^MListItem#C<§T#G§>)?\;
                ERROR: Unknown local var i : uint = <l>index\uint\;
                ERROR: Unknown while(((<lm>c\(^MListItem#C<§T#G§>)?\ != none<^MListItem#C<§T#G§>>) && (<lm>i\uint\ > 0))) {
    (<lm>c\(^MListItem#C<§T#G§>)¿\ = <lm>c\(^MListItem#C<§T#G§>)¿\.<eIm>next\(^MListItem#C<§T#G§>)?\)
};
                if(c != null) {
                    ERROR: Unknown local li : MListItem#C<§T#G§> = <to>MListItem\MListItem#C.class\.<tcI>apply(data = <l>item\§T#G§\)\MListItem#C<§T#G§>\;
                    {
                        ERROR: Unknown local __tmp_0_3_1 : (^MListItem#C<§T#G§>)? = <lm>c\(^MListItem#C<§T#G§>)¿\.<eIm>next\(^MListItem#C<§T#G§>)?\;
                        if(__tmp_0_3_1 != null) {
                            __tmp_0_3_1.prev = li;
                        }
                    }
                    c.next = li;
                } else {
                    appendItem(item);
                }
            }
        }
    }
    @Override
    public void prependItem(T item) {
        ERROR: Unknown local i : MListItem#C<§T#G§> = <to>MListItem\MListItem#C.class\.<tcI>apply(data = <l>item\§T#G§\)\MListItem#C<§T#G§>\;
        if(this.headItem == null) {
            this.headItem = i;
            this.lastItem = i;
            this._count = ERROR: Unknown 1.cast<uint>;
        } else {
            i.next = ERROR: Unknown <MList#C<T#G>>self.<emp>headItem\(^MListItem#C<§T#G§>)¿\.cast<(^MListItem#C<§T#G§>)?>;
            this.headItem.prev = i;
            this.headItem = i;
            ERROR: Unknown <MList#C<T#G>>self.<emp>_count\uint\++;
        }
    }
    @Override
    public void appendItem(T item) {
        ERROR: Unknown local i : MListItem#C<§T#G§> = <to>MListItem\MListItem#C.class\.<tcI>apply(data = <l>item\§T#G§\)\MListItem#C<§T#G§>\;
        if(this.lastItem == null) {
            this.headItem = i;
            this.lastItem = i;
            this._count = ERROR: Unknown 1.cast<uint>;
        } else {
            i.prev = ERROR: Unknown <MList#C<T#G>>self.<emp>lastItem\(^MListItem#C<§T#G§>)¿\.cast<(^MListItem#C<§T#G§>)?>;
            this.lastItem.next = i;
            this.lastItem = i;
            ERROR: Unknown <MList#C<T#G>>self.<emp>_count\uint\++;
        }
    }
    public void removeListItem(MListItem<T> listItem) {
        if(this.headItem != null && this.headItem.equals(listItem)) {
            this.headItem = this.headItem.next;
            this.headItem.prev = null;
        } else {
            if(this.lastItem != null && this.lastItem.equals(listItem)) {
                this.lastItem = this.lastItem.prev;
                this.lastItem.next = null;
            } else {
                {
                    ERROR: Unknown local __tmp_0_0 : (^MListItem#C<§T#G§>)? = <l>listItem\MListItem#C<§T#G§>\.<eImw>prev\(^MListItem#C<§T#G§>)?\;
                    if(__tmp_0_0 != null) {
                        __tmp_0_0.next = listItem.next;
                    }
                }
                {
                    ERROR: Unknown local __tmp_0_1 : (^MListItem#C<§T#G§>)? = <l>listItem\MListItem#C<§T#G§>\.<eIm>next\(^MListItem#C<§T#G§>)?\;
                    if(__tmp_0_1 != null) {
                        __tmp_0_1.prev = listItem.prev;
                    }
                }
            }
        }
        ERROR: Unknown <MList#C<T#G>>self.<emp>_count\uint\--;
    }
    @Override
    public void clear() {
        this.headItem = null;
        this.lastItem = null;
    }
    public void removeHead() {
        {
            ERROR: Unknown local _ : ^(^MListItem#C<§T#G§>)¿ = <MList#C<T#G>>self.<emp>headItem\(^MListItem#C<§T#G§>)?\;
            if(_ != null) {
                removeListItem(_);
            }
        }
    }
    public void removeLast() {
        {
            ERROR: Unknown local _ : ^(^MListItem#C<§T#G§>)¿ = <MList#C<T#G>>self.<emp>lastItem\(^MListItem#C<§T#G§>)?\;
            if(_ != null) {
                removeListItem(_);
            }
        }
    }
    public T takeHead() {
        ERROR: Unknown local h : ^(^MListItem#C<§T#G§>)¿ = <MList#C<T#G>>self.<emp>headItem\(^MListItem#C<§T#G§>)?\;
        if(h != null) {
            ERROR: Unknown local r : T#G = <l>h\^(^MListItem#C<§T#G§>)¿\.<eIUm>data\§T#G§\;
            removeListItem(h);
            return r;
        } else {
            return null;
        }
    }
    @Override
    public T last() {
        return ERROR: Unknown <MList#C<T#G>>self.<emp>lastItem\(^MListItem#C<§T#G§>)?\?.<eIUm>data\§T#G§\;
    }
    public T takeLast() {
        ERROR: Unknown local h : ^(^MListItem#C<§T#G§>)¿ = <MList#C<T#G>>self.<emp>lastItem\(^MListItem#C<§T#G§>)?\;
        if(h != null) {
            ERROR: Unknown local r : T#G = <l>h\^(^MListItem#C<§T#G§>)¿\.<eIUm>data\§T#G§\;
            removeListItem(h);
            return r;
        } else {
            return null;
        }
    }
    @Override
    public void forEach(P<T> each) {
        ERROR: Unknown local var i : (^MListItem#C<§T#G§>)? = <MList#C<T#G>>self.<emp>headItem\(^MListItem#C<§T#G§>)?\;
        ERROR: Unknown while((<lm>i\(^MListItem#C<§T#G§>)?\ != none<^MListItem#C<§T#G§>>)) {
    <l>each\§T#G§ -> void\.<d>apply( = <lm>i\(^MListItem#C<§T#G§>)¿\.<eIUm>data\§T#G§\)\void\
    (<lm>i\(^MListItem#C<§T#G§>)¿\ = <lm>i\(^MListItem#C<§T#G§>)¿\.<eIm>next\(^MListItem#C<§T#G§>)?\)
};
    }
    @Override
    public boolean goOn(F<T, Boolean> on) {
        ERROR: Unknown local var i : (^MListItem#C<§T#G§>)? = <MList#C<T#G>>self.<emp>headItem\(^MListItem#C<§T#G§>)?\;
        ERROR: Unknown while((<lm>i\(^MListItem#C<§T#G§>)?\ != none<^MListItem#C<§T#G§>>)) {
    if(!(<l>on\§T#G§ -> bool\.<d>apply( = <lm>i\(^MListItem#C<§T#G§>)¿\.<eIUm>data\§T#G§\)\bool\)) return False
    (<lm>i\(^MListItem#C<§T#G§>)¿\ = <lm>i\(^MListItem#C<§T#G§>)¿\.<eIm>next\(^MListItem#C<§T#G§>)?\)
};
        return ERROR: Unknown True;
    }
    @Override
    public void mutableFilterBy(F<T, Boolean> by) {
        ERROR: Unknown local var i : (^MListItem#C<§T#G§>)? = <MList#C<T#G>>self.<emp>headItem\(^MListItem#C<§T#G§>)?\;
        ERROR: Unknown while((<lm>i\(^MListItem#C<§T#G§>)?\ != none<^MListItem#C<§T#G§>>)) {
    if(!(<l>by\§T#G§ -> bool\.<d>apply( = <lm>i\(^MListItem#C<§T#G§>)¿\.<eIUm>data\§T#G§\)\bool\)) <MList#C<T#G>>self.<dI>remove(listItem = <lm>i\(^MListItem#C<§T#G§>)¿\)\void\
    (<lm>i\(^MListItem#C<§T#G§>)¿\ = <lm>i\(^MListItem#C<§T#G§>)¿\.<eIm>next\(^MListItem#C<§T#G§>)?\)
};
    }
    @Override
    public T head() {
        return ERROR: Unknown <MList#C<T#G>>self.<emp>headItem\(^MListItem#C<§T#G§>)?\?.<eIUm>data\§T#G§\;
    }
    public MList() {
    }
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
        return ret;
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
        throw new RuntimeException("Incorrect index");
    }
    @Override
    public ImSeq<T> im() {
        return this.imCopy();
    }
    @Override
    public ImSeq<T> imCopy() {
        ERROR: Unknown local arr : MArray#C<§T#G§> = <to>MArray\MArray#C.class\.<tcI>apply\MArray#C<§T#G§>\;
        forEach(new P<T>() {
            @Override
            public void apply(T item) {
                arr.appendItem(item);
            }
        });
        return arr.im();
    }
    public T applyIndex(int index) {
        if(index >= this.count()) {
            return null;
        }
        ERROR: Unknown local i : Iterator#T<§T#G§> = <Seq#T<T#G>>self.<rdIa>iterator\Iterator#T<§T#G§>\;
        ERROR: Unknown local var n : uint = <l>index\uint\;
        ERROR: Unknown while(<l>i\Iterator#T<§T#G§>\.<dIa>hasNext\bool\) {
    if((<lm>n\uint\ == 0)) return <l>i\Iterator#T<§T#G§>\.<dIa>next\§T#G§\
    <l>i\Iterator#T<§T#G§>\.<dIa>next\§T#G§\
    <lm>n\uint\--
};
        return null;
    }
    public Set<T> toSet() {
        return convertWithBuilder<Set<T>>(new HashSetBuilder<T>());
    }
    public boolean isEqualSeq(Seq<T> seq) {
        if(this.count().equals(seq.count())) {
            return ERROR: Unknown False;
        }
        ERROR: Unknown local ia : Iterator#T<§T#G§> = <Seq#T<T#G>>self.<rdIa>iterator\Iterator#T<§T#G§>\;
        ERROR: Unknown local ib : Iterator#T<§T#G§> = <l>seq\Seq#T<§T#G§>\.<rdIa>iterator\Iterator#T<§T#G§>\;
        ERROR: Unknown while((<l>ia\Iterator#T<§T#G§>\.<dIa>hasNext\bool\ && <l>ib\Iterator#T<§T#G§>\.<dIa>hasNext\bool\)) {
    if((<l>ia\Iterator#T<§T#G§>\.<dIa>next\§T#G§\ != <l>ib\Iterator#T<§T#G§>\.<dIa>next\§T#G§\)) return False
};
        return ERROR: Unknown True;
    }
    @Override
    public boolean isEmpty() {
        return this.count().equals(ERROR: Unknown 0);
    }
    public ImSeq<T> tail() {
        ERROR: Unknown local builder : ArrayBuilder#C<§T#G§> = <to>ArrayBuilder\ArrayBuilder#C.class\.<tcI>apply\ArrayBuilder#C<§T#G§>\;
        ERROR: Unknown local i : Iterator#T<§T#G§> = <Seq#T<T#G>>self.<rdIa>iterator\Iterator#T<§T#G§>\;
        if(i.hasNext()) {
            i.next();
            ERROR: Unknown while(<l>i\Iterator#T<§T#G§>\.<dIa>hasNext\bool\) {
    <l>builder\ArrayBuilder#C<§T#G§>\.<dIo>append(item = <l>i\Iterator#T<§T#G§>\.<dIa>next\§T#G§\)\void\
};
        }
        return builder.build();
    }
    public boolean isEmpty() {
        return ERROR: Unknown !(<Iterable#T<T#G>>self.<dIa>iterator\Iterator#T<§T#G§>\.<dIa>hasNext\bool\);
    }
    @Override
    public void parForEach(P<T> each) {
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
    public boolean removeItem(T item) {
        ERROR: Unknown local i : MIterator#T<§T#G§> = <MIterable#T<T#G>>self.<dIa>mutableIterator\MIterator#T<§T#G§>\;
        ERROR: Unknown local var ret : bool = False;
        ERROR: Unknown while(<l>i\MIterator#T<§T#G§>\.<rdIa>hasNext\bool\) {
    if((<l>i\MIterator#T<§T#G§>\.<rdIa>next\§T#G§\ == <l>item\§T#G§\)) {
    <l>i\MIterator#T<§T#G§>\.<dIa>remove\void\
    (<lm>ret\bool\ = True)
}
};
        return ret;
    }
    @Override
    public ImIterable<T> im() {
        return this.imCopy();
    }
    @Override
    public ImIterable<T> imCopy() {
        ERROR: Unknown local arr : MArray#C<§T#G§> = <to>MArray\MArray#C.class\.<tcI>apply\MArray#C<§T#G§>\;
        forEach(new P<T>() {
            @Override
            public void apply(T item) {
                arr.appendItem(item);
            }
        });
        return arr.im();
    }
    public boolean isEmpty() {
        return ERROR: Unknown !(<Iterable#T<T#G>>self.<dIa>iterator\Iterator#T<§T#G§>\.<dIa>hasNext\bool\);
    }
    @Override
    public void parForEach(P<T> each) {
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
    public C convertWithBuilder(Builder<T, C> builder) {
        forEach(new P<T>() {
            @Override
            public void apply(T x) {
                builder.appendItem(x);
            }
        });
        return builder.build();
    }
    public ImTraversable<T> im() {
        return this.imCopy();
    }
    public ImTraversable<T> imCopy() {
        ERROR: Unknown local arr : MArray#C<§T#G§> = <to>MArray\MArray#C.class\.<tcI>apply\MArray#C<§T#G§>\;
        forEach(new P<T>() {
            @Override
            public void apply(T item) {
                arr.appendItem(item);
            }
        });
        return arr.im();
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