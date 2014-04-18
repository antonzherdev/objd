package core.chain;

public class Range implements ImSeq<int> {
    public final int start;
    public final int end;
    public final int step;
    public final int count = ERROR: Unknown if((<Range#C>self.<eIU>step\int\ > 0)) if((<Range#C>self.<eIU>start\int\ <= <Range#C>self.<eIU>end\int\)) (((<Range#C>self.<eIU>end\int\ - <Range#C>self.<eIU>start\int\) / <Range#C>self.<eIU>step\int\) + 1).cast<uint>
else 0.cast<uint>
else if((<Range#C>self.<eIU>step\int\ < 0)) if((<Range#C>self.<eIU>start\int\ >= <Range#C>self.<eIU>end\int\)) (((<Range#C>self.<eIU>end\int\ - <Range#C>self.<eIU>start\int\) / <Range#C>self.<eIU>step\int\) + 1).cast<uint>
else 0.cast<uint>
else 1.cast<uint>;
    @Override
    public Integer applyIndex(int index) {
        ERROR: Unknown if((<l>index\uint\ < <Range#C>self.<eIo>count\uint\)) return some((<Range#C>self.<eIU>start\int\ + (<Range#C>self.<eIU>step\int\ * <l>index\uint\)))\(^int)?\
else return none<^int>;
    }
    @Override
    public Iterator<Integer> iterator() {
        return new RangeIterator(start, end, step);
    }
    public Range setStep(int step) {
        return new Range(start, end, step);
    }
    @Override
    public boolean isEmpty() {
        ERROR: Unknown if((<Range#C>self.<eIU>step\int\ > 0)) return (<Range#C>self.<eIU>start\int\ > <Range#C>self.<eIU>end\int\)
else if((<Range#C>self.<eIU>step\int\ < 0)) return (<Range#C>self.<eIU>start\int\ < <Range#C>self.<eIU>end\int\)
else return False;
    }
    public static Range applyI(int i) {
        return new Range(i, i, ERROR: Unknown 1);
    }
    public Range(int start,int end,int step) {
    }
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
        return chain().filter(new F<T, Boolean>() {
            @Override
            public Boolean f(T _) {
                return ERROR: Unknown (<l>_\§T#G§\ != <l>item\§T#G§\);
            }
        }).toArray();
    }
    @Override
    public MSeq<T> mCopy() {
        ERROR: Unknown local arr : MArray#C<§T#G§> = <to>MArray\MArray#C.class\.<tcI>apply\MArray#C<§T#G§>\;
        forEach(new P<T>() {
            @Override
            public void f(T item) {
                arr.appendItem(item);
            }
        });
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
    @Override
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
    @Override
    public T head() {
        ERROR: Unknown if(<Iterable#T<T#G>>self.<dI>isEmpty\bool\) return none<T#G>
else return some(<Iterable#T<T#G>>self.<dIa>iterator\Iterator#T<§T#G§>\.<dIa>next\§T#G§\)\§(T#G)?§\;
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
    <to>DispatchQueue\DispatchQueue#C.class\.<eIt>default\DispatchQueue#C\.<dI>async(f =  -> void = <l>each\§T#G§ -> void\.<d>apply( = <l>v\§T#G§\)\void\)\void\
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
            public Boolean f(T item) {
                each.apply(item);
                return ERROR: Unknown True;
            }
        });
    }
    public void parForEach(P<T> each) {
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean f(T item) {
                DispatchQueue().default.asyncF(new P0() {
                    @Override
                    public void f() {
                        each.apply(item);
                    }
                });
                return ERROR: Unknown True;
            }
        });
    }
    public Chain<T> chain() {
        return Chain().chainWithCollection<T>(ERROR: Unknown <Traversable#T<T#G>>self);
    }
    public T findWhere(F<T, Boolean> where) {
        ERROR: Unknown local var ret : (T#G)? = none<T#G>;
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean f(T x) {
                ERROR: Unknown if(<l>where\§T#G§ -> bool\.<d>apply( = <l>x\§T#G§\)\bool\) {
    (<lm>ret\(§T#G§)?\ = some(<l>x\§T#G§\)\§(T#G)?§\)
    return False
}
else return True;
            }
        });
        return ret;
    }
    public boolean existsWhere(F<T, Boolean> where) {
        ERROR: Unknown local var ret : bool = False;
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean f(T x) {
                ERROR: Unknown if(<l>where\§T#G§ -> bool\.<d>apply( = <l>x\§T#G§\)\bool\) {
    (<lm>ret\bool\ = True)
    return False
}
else return True;
            }
        });
        return ret;
    }
    public boolean allConfirm(F<T, Boolean> confirm) {
        ERROR: Unknown local var ret : bool = True;
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean f(T x) {
                ERROR: Unknown if(!(<l>confirm\§T#G§ -> bool\.<d>apply( = <l>x\§T#G§\)\bool\)) {
    (<lm>ret\bool\ = False)
    return False
}
else return True;
            }
        });
        return ret;
    }
    public T head() {
        ERROR: Unknown local var ret : (T#G)? = ;
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean f(T on) {
                ERROR: Unknown (<lm>ret\(§T#G§)?\ = some(<l>on\§T#G§\)\§(T#G)?§\);
                return ERROR: Unknown False;
            }
        });
        return ret;
    }
    public C convertWithBuilder(Builder<T, C> builder) {
        forEach(new P<T>() {
            @Override
            public void f(T x) {
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
            public void f(T item) {
                arr.appendItem(item);
            }
        });
        return arr;
    }
    @Override
    public T head() {
        ERROR: Unknown if(<Iterable#T<T#G>>self.<dI>isEmpty\bool\) return none<T#G>
else return some(<Iterable#T<T#G>>self.<dIa>iterator\Iterator#T<§T#G§>\.<dIa>next\§T#G§\)\§(T#G)?§\;
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
    <to>DispatchQueue\DispatchQueue#C.class\.<eIt>default\DispatchQueue#C\.<dI>async(f =  -> void = <l>each\§T#G§ -> void\.<d>apply( = <l>v\§T#G§\)\void\)\void\
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
            public Boolean f(T item) {
                each.apply(item);
                return ERROR: Unknown True;
            }
        });
    }
    public void parForEach(P<T> each) {
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean f(T item) {
                DispatchQueue().default.asyncF(new P0() {
                    @Override
                    public void f() {
                        each.apply(item);
                    }
                });
                return ERROR: Unknown True;
            }
        });
    }
    public Chain<T> chain() {
        return Chain().chainWithCollection<T>(ERROR: Unknown <Traversable#T<T#G>>self);
    }
    public T findWhere(F<T, Boolean> where) {
        ERROR: Unknown local var ret : (T#G)? = none<T#G>;
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean f(T x) {
                ERROR: Unknown if(<l>where\§T#G§ -> bool\.<d>apply( = <l>x\§T#G§\)\bool\) {
    (<lm>ret\(§T#G§)?\ = some(<l>x\§T#G§\)\§(T#G)?§\)
    return False
}
else return True;
            }
        });
        return ret;
    }
    public boolean existsWhere(F<T, Boolean> where) {
        ERROR: Unknown local var ret : bool = False;
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean f(T x) {
                ERROR: Unknown if(<l>where\§T#G§ -> bool\.<d>apply( = <l>x\§T#G§\)\bool\) {
    (<lm>ret\bool\ = True)
    return False
}
else return True;
            }
        });
        return ret;
    }
    public boolean allConfirm(F<T, Boolean> confirm) {
        ERROR: Unknown local var ret : bool = True;
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean f(T x) {
                ERROR: Unknown if(!(<l>confirm\§T#G§ -> bool\.<d>apply( = <l>x\§T#G§\)\bool\)) {
    (<lm>ret\bool\ = False)
    return False
}
else return True;
            }
        });
        return ret;
    }
    public T head() {
        ERROR: Unknown local var ret : (T#G)? = ;
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean f(T on) {
                ERROR: Unknown (<lm>ret\(§T#G§)?\ = some(<l>on\§T#G§\)\§(T#G)?§\);
                return ERROR: Unknown False;
            }
        });
        return ret;
    }
    public C convertWithBuilder(Builder<T, C> builder) {
        forEach(new P<T>() {
            @Override
            public void f(T x) {
                builder.appendItem(x);
            }
        });
        return builder.build();
    }
    public MTraversable<T> mCopy() {
        ERROR: Unknown local arr : MArray#C<§T#G§> = <to>MArray\MArray#C.class\.<tcI>apply\MArray#C<§T#G§>\;
        forEach(new P<T>() {
            @Override
            public void f(T item) {
                arr.appendItem(item);
            }
        });
        return arr;
    }
    public void forEach(P<T> each) {
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean f(T item) {
                each.apply(item);
                return ERROR: Unknown True;
            }
        });
    }
    public void parForEach(P<T> each) {
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean f(T item) {
                DispatchQueue().default.asyncF(new P0() {
                    @Override
                    public void f() {
                        each.apply(item);
                    }
                });
                return ERROR: Unknown True;
            }
        });
    }
    public Chain<T> chain() {
        return Chain().chainWithCollection<T>(ERROR: Unknown <Traversable#T<T#G>>self);
    }
    public T findWhere(F<T, Boolean> where) {
        ERROR: Unknown local var ret : (T#G)? = none<T#G>;
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean f(T x) {
                ERROR: Unknown if(<l>where\§T#G§ -> bool\.<d>apply( = <l>x\§T#G§\)\bool\) {
    (<lm>ret\(§T#G§)?\ = some(<l>x\§T#G§\)\§(T#G)?§\)
    return False
}
else return True;
            }
        });
        return ret;
    }
    public boolean existsWhere(F<T, Boolean> where) {
        ERROR: Unknown local var ret : bool = False;
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean f(T x) {
                ERROR: Unknown if(<l>where\§T#G§ -> bool\.<d>apply( = <l>x\§T#G§\)\bool\) {
    (<lm>ret\bool\ = True)
    return False
}
else return True;
            }
        });
        return ret;
    }
    public boolean allConfirm(F<T, Boolean> confirm) {
        ERROR: Unknown local var ret : bool = True;
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean f(T x) {
                ERROR: Unknown if(!(<l>confirm\§T#G§ -> bool\.<d>apply( = <l>x\§T#G§\)\bool\)) {
    (<lm>ret\bool\ = False)
    return False
}
else return True;
            }
        });
        return ret;
    }
    public T head() {
        ERROR: Unknown local var ret : (T#G)? = ;
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean f(T on) {
                ERROR: Unknown (<lm>ret\(§T#G§)?\ = some(<l>on\§T#G§\)\§(T#G)?§\);
                return ERROR: Unknown False;
            }
        });
        return ret;
    }
    public C convertWithBuilder(Builder<T, C> builder) {
        forEach(new P<T>() {
            @Override
            public void f(T x) {
                builder.appendItem(x);
            }
        });
        return builder.build();
    }
}