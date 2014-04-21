package core.chain;

public class ImList<T> implements ImSeq<T> {
    public static ImList<T> apply() {
        return EmptyList().instance.ERROR: Unknown cast<ImList#C<T#G>>;
    }
    public static ImList<T> applyItem(T item) {
        return new FilledList<T>(item, EmptyList().instance);
    }
    public static ImList<T> applyItemTail(T item,ImList<T> tail) {
        return new FilledList<T>(item, tail);
    }
    @Override
    public Iterator<T> iterator() {
        ERROR: Unknown local i : ListIterator#C<§T#G§> = <to>ListIterator\ListIterator#C.class\.<tcI>apply\ListIterator#C<§T#G§>\;
        i.list = this;
        return i;
    }
    @Override
    public abstract ImList<T> tail();
    public abstract ImList<T> filterF(F<T, Boolean> f);
    public abstract ImList<T> reverse();
    public abstract ImList<T> insertItem(C item);
    public ImList() {
    }
    public ImSeq<T> addItem(T item) {
        ERROR: Unknown local builder : ArrayBuilder#C<§T#G§> = <to>ArrayBuilder\ArrayBuilder#C.class\.<tcI>apply\ArrayBuilder#C<§T#G§>\;
        builder.appendAllItems(this);
        builder.appendItem(item);
        return builder.build();
    }
    public ImSeq<T> addSeq(Seq<T> seq) {
        ERROR: Unknown local builder : ArrayBuilder#C<§T#G§> = <to>ArrayBuilder\ArrayBuilder#C.class\.<tcI>apply\ArrayBuilder#C<§T#G§>\;
        builder.appendAllItems(this);
        builder.appendAllItems(seq);
        return builder.build();
    }
    public ImSeq<T> subItem(T item) {
        return this.chain().filter(new F<T, Boolean>() {
            @Override
            public Boolean apply(T _) {
                return _.equals(item);
            }
        }).toArray();
    }
    @Override
    public MSeq<T> mCopy() {
        ERROR: Unknown local arr : MArray#C<§T#G§> = <to>MArray\MArray#C.class\.<tcI>apply\MArray#C<§T#G§>\;
        forEach(new P<T>() {
            @Override
            public void apply(T item) {
                arr.appendItem(item);
            }
        });
        return arr;
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
    @Override
    public T head() {
        return applyIndex(ERROR: Unknown 0.cast<uint>);
    }
    public T last() {
        return applyIndex(this.count() - ERROR: Unknown 1);
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
    public int count() {
        ERROR: Unknown local i : Iterator#T<§T#G§> = <Iterable#T<T#G>>self.<dIa>iterator\Iterator#T<§T#G§>\;
        ERROR: Unknown local var n : uint = 0.cast<uint>;
        ERROR: Unknown while(<l>i\Iterator#T<§T#G§>\.<dIa>hasNext\bool\) {
    <l>i\Iterator#T<§T#G§>\.<dIa>next\§T#G§\
    <lm>n\uint\++
};
        return n;
    }
    @Override
    public T head() {
        if(this.isEmpty()) {
            return null;
        } else {
            return this.iterator().next();
        }
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
    <to>DispatchQueue\DispatchQueue#C.class\.<eIt>default\DispatchQueue#C\.<dIb>async(f =  -> void = <l>each\§T#G§ -> void\.<d>apply( = <l>v\§T#G§\)\void\)\void\
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
            public Boolean apply(T item) {
                each.apply(item);
                return ERROR: Unknown True;
            }
        });
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
    public T head() {
        ERROR: Unknown local var ret : (T#G)? = ;
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean apply(T on) {
                ret = on;
                return ERROR: Unknown False;
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
    public MIterable<T> mCopy() {
        ERROR: Unknown local arr : MArray#C<§T#G§> = <to>MArray\MArray#C.class\.<tcI>apply\MArray#C<§T#G§>\;
        forEach(new P<T>() {
            @Override
            public void apply(T item) {
                arr.appendItem(item);
            }
        });
        return arr;
    }
    public int count() {
        ERROR: Unknown local i : Iterator#T<§T#G§> = <Iterable#T<T#G>>self.<dIa>iterator\Iterator#T<§T#G§>\;
        ERROR: Unknown local var n : uint = 0.cast<uint>;
        ERROR: Unknown while(<l>i\Iterator#T<§T#G§>\.<dIa>hasNext\bool\) {
    <l>i\Iterator#T<§T#G§>\.<dIa>next\§T#G§\
    <lm>n\uint\++
};
        return n;
    }
    @Override
    public T head() {
        if(this.isEmpty()) {
            return null;
        } else {
            return this.iterator().next();
        }
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
    <to>DispatchQueue\DispatchQueue#C.class\.<eIt>default\DispatchQueue#C\.<dIb>async(f =  -> void = <l>each\§T#G§ -> void\.<d>apply( = <l>v\§T#G§\)\void\)\void\
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
            public Boolean apply(T item) {
                each.apply(item);
                return ERROR: Unknown True;
            }
        });
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
    public T head() {
        ERROR: Unknown local var ret : (T#G)? = ;
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean apply(T on) {
                ret = on;
                return ERROR: Unknown False;
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
    public MTraversable<T> mCopy() {
        ERROR: Unknown local arr : MArray#C<§T#G§> = <to>MArray\MArray#C.class\.<tcI>apply\MArray#C<§T#G§>\;
        forEach(new P<T>() {
            @Override
            public void apply(T item) {
                arr.appendItem(item);
            }
        });
        return arr;
    }
    public void forEach(P<T> each) {
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean apply(T item) {
                each.apply(item);
                return ERROR: Unknown True;
            }
        });
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
    public T head() {
        ERROR: Unknown local var ret : (T#G)? = ;
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean apply(T on) {
                ret = on;
                return ERROR: Unknown False;
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