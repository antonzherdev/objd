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
        MListImmutableIterator<T> i = new MListImmutableIterator<T>();
        i.item = this.headItem;
        return i;
    }
    @Override
    public MIterator<T> mutableIterator() {
        MListIterator<T> i = new MListIterator<T>(this);
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
                MListItem<T> c = this.headItem;
                int i = index;
                ERROR: Unknown while(((<lm>c\(^MListItem#C<§T#G§>)?\ != none<^MListItem#C<§T#G§>>) && (<lm>i\uint\ > 0))) {
    (<lm>c\(^MListItem#C<§T#G§>)¿\ = <lm>c\(^MListItem#C<§T#G§>)¿\.<eIm>next\(^MListItem#C<§T#G§>)?\)
};
                if(c != null) {
                    MListItem<T> li = new MListItem<T>(item);
                    {
                        MListItem<T> __tmp_0_3_1 = c.next;
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
        MListItem<T> i = new MListItem<T>(item);
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
        MListItem<T> i = new MListItem<T>(item);
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
                    MListItem<T> __tmp_0_0 = listItem.prev;
                    if(__tmp_0_0 != null) {
                        __tmp_0_0.next = listItem.next;
                    }
                }
                {
                    MListItem<T> __tmp_0_1 = listItem.next;
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
            MListItem<T> _ = this.headItem;
            if(_ != null) {
                removeListItem(_);
            }
        }
    }
    public void removeLast() {
        {
            MListItem<T> _ = this.lastItem;
            if(_ != null) {
                removeListItem(_);
            }
        }
    }
    public T takeHead() {
        MListItem<T> h = this.headItem;
        if(h != null) {
            T r = h.data;
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
        MListItem<T> h = this.lastItem;
        if(h != null) {
            T r = h.data;
            removeListItem(h);
            return r;
        } else {
            return null;
        }
    }
    @Override
    public void forEach(P<T> each) {
        MListItem<T> i = this.headItem;
        ERROR: Unknown while((<lm>i\(^MListItem#C<§T#G§>)?\ != none<^MListItem#C<§T#G§>>)) {
    <l>each\§T#G§ -> void\.<d>apply( = <lm>i\(^MListItem#C<§T#G§>)¿\.<eIUm>data\§T#G§\)\void\
    (<lm>i\(^MListItem#C<§T#G§>)¿\ = <lm>i\(^MListItem#C<§T#G§>)¿\.<eIm>next\(^MListItem#C<§T#G§>)?\)
};
    }
    @Override
    public boolean goOn(F<T, Boolean> on) {
        MListItem<T> i = this.headItem;
        ERROR: Unknown while((<lm>i\(^MListItem#C<§T#G§>)?\ != none<^MListItem#C<§T#G§>>)) {
    if(!(<l>on\§T#G§ -> bool\.<d>apply( = <lm>i\(^MListItem#C<§T#G§>)¿\.<eIUm>data\§T#G§\)\bool\)) return False
    (<lm>i\(^MListItem#C<§T#G§>)¿\ = <lm>i\(^MListItem#C<§T#G§>)¿\.<eIm>next\(^MListItem#C<§T#G§>)?\)
};
        return ERROR: Unknown True;
    }
    @Override
    public void mutableFilterBy(F<T, Boolean> by) {
        MListItem<T> i = this.headItem;
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
        MIterator<T> i = this.mutableIterator();
        int j = index;
        boolean ret = ERROR: Unknown False;
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
        MIterator<T> i = this.mutableIterator();
        int n = index;
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
        MArray<T> arr = new MArray<T>();
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
        Iterator<T> i = this.iterator();
        int n = index;
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
        Iterator<T> ia = this.iterator();
        Iterator<T> ib = seq.iterator();
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
        ArrayBuilder<T> builder = new ArrayBuilder<T>();
        Iterator<T> i = this.iterator();
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
        Iterator<T> i = this.iterator();
        ERROR: Unknown while(<l>i\Iterator#T<§T#G§>\.<dIa>hasNext\bool\) {
    local v : T#G = <l>i\Iterator#T<§T#G§>\.<dIa>next\§T#G§\
    <to>DispatchQueue\DispatchQueue#C.class\.<eIt>default\DispatchQueue#C\.<dIb>async(f =  -> void = <l>each\§T#G§ -> void\.<d>apply( = <l>v\§T#G§\)\void\)\void\
};
    }
    public boolean containsItem(T item) {
        Iterator<T> i = this.iterator();
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
        T ret = null;
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
        boolean ret = ERROR: Unknown False;
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
        boolean ret = ERROR: Unknown True;
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
        MIterator<T> i = this.mutableIterator();
        boolean ret = ERROR: Unknown False;
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
        MArray<T> arr = new MArray<T>();
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
        Iterator<T> i = this.iterator();
        ERROR: Unknown while(<l>i\Iterator#T<§T#G§>\.<dIa>hasNext\bool\) {
    local v : T#G = <l>i\Iterator#T<§T#G§>\.<dIa>next\§T#G§\
    <to>DispatchQueue\DispatchQueue#C.class\.<eIt>default\DispatchQueue#C\.<dIb>async(f =  -> void = <l>each\§T#G§ -> void\.<d>apply( = <l>v\§T#G§\)\void\)\void\
};
    }
    public boolean containsItem(T item) {
        Iterator<T> i = this.iterator();
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
        T ret = null;
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
        boolean ret = ERROR: Unknown False;
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
        boolean ret = ERROR: Unknown True;
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
        MArray<T> arr = new MArray<T>();
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
        T ret = null;
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
        boolean ret = ERROR: Unknown False;
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
        boolean ret = ERROR: Unknown True;
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