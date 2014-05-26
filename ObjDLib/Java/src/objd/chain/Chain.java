package objd.chain;

import objd.lang.*;
import objd.collection.Traversable;
import objd.collection.ImArray;
import objd.collection.ArrayBuilder;
import objd.collection.Builder;
import objd.collection.Iterable;
import objd.collection.Iterator;
import objd.collection.Go;
import objd.collection.Seq;
import objd.collection.MArray;
import objd.collection.ImList;
import objd.collection.ImListBuilder;
import objd.collection.Set;
import objd.collection.HashSetBuilder;
import objd.collection.TreeSet;
import objd.collection.ImTreeSet;
import objd.collection.TreeSetBuilder;
import objd.collection.ImHashMap;
import objd.collection.HashMapBuilder;
import objd.collection.MHashMap;
import objd.collection.StringBuilder;
import objd.concurrent.Future;
import objd.collection.ImTraversable_impl;

public class Chain<A> extends ImTraversable_impl<A> {
    public final ChainLink<?, A> link;
    public final Chain<?> previous;
    public static <T> Chain<T> applyCollection(final Traversable<T> collection) {
        return new Chain<T>(((ChainLink)(new SourceLink<T>(collection))), null);
    }
    public Chain<A> filterFactorWhen(final double factor, final F<A, Boolean> when) {
        return this.<A>addLink(((ChainLink<A, A>)(((ChainLink)(new FilterLink<A>(factor, when))))));
    }
    public Chain<A> filterWhen(final F<A, Boolean> when) {
        return filterFactorWhen(0.5, when);
    }
    public Chain<A> topNumbers(final int numbers) {
        return this.<A>addLink(((ChainLink<A, A>)(((ChainLink)(new TopLink<A>(((int)(numbers))))))));
    }
    public <B> Chain<B> filterCastFactorTo(final double factor, final ClassType<B> to) {
        return ((Chain<B>)(((Chain)(this.<A>addLink(((ChainLink<A, A>)(((ChainLink)(new FilterLink<A>(factor, new F<A, Boolean>() {
            @Override
            public Boolean apply(final A item) {
                return to.isInstanceObj(item);
            }
        }))))))))));
    }
    public <B> Chain<B> filterCastTo(final ClassType<B> to) {
        return this.<B>filterCastFactorTo(0.5, to);
    }
    public <B> Chain<B> mapF(final F<A, B> f) {
        return this.<B>addLink(new MapLink<A, B>(f));
    }
    public <B> Chain<B> mapOptF(final F<A, B> f) {
        return this.<B>addLink(new MapOptLink<A, B>(((F<A, B>)(((F)(f))))));
    }
    public <B> Chain<B> flatMapFactorF(final double factor, final F<A, Traversable<B>> f) {
        return this.<B>addLink(new FlatMapLink<A, B>(factor, f));
    }
    public <B> Chain<B> flatMapF(final F<A, Traversable<B>> f) {
        return this.<B>flatMapFactorF(2.0, f);
    }
    public <B> Chain<B> flatFactor(final double factor) {
        return ((Chain<Traversable<B>>)(((Chain)(this)))).<B>addLink(((ChainLink<Traversable<B>, B>)(((ChainLink)(new FlatLink<B>(factor))))));
    }
    public <B> Chain<B> flat() {
        return this.<B>flatFactor(2.0);
    }
    public Chain<Tuple<A, A>> combinations() {
        return ((Chain<Tuple<A, A>>)(((Chain)(this.<Tuple<A, A>>addLink(((ChainLink<A, Tuple<A, A>>)(((ChainLink)(new CombinationsLink<A>())))))))));
    }
    public <B> Chain<B> uncombinations() {
        return ((Chain<Tuple<B, B>>)(((Chain)(this)))).<B>addLink(((ChainLink<Tuple<B, B>, B>)(((ChainLink)(new UncombinationsLink<B>())))));
    }
    public Chain<Tuple<A, A>> neighbours() {
        return ((Chain<Tuple<A, A>>)(((Chain)(this.<Tuple<A, A>>addLink(((ChainLink<A, Tuple<A, A>>)(((ChainLink)(new NeighboursLink<A>(false))))))))));
    }
    public Chain<Tuple<A, A>> neighboursRing() {
        return ((Chain<Tuple<A, A>>)(((Chain)(this.<Tuple<A, A>>addLink(((ChainLink<A, Tuple<A, A>>)(((ChainLink)(new NeighboursLink<A>(true))))))))));
    }
    public <B> Chain<Tuple<A, B>> mulBy(final Traversable<B> by) {
        return ((Chain<Tuple<A, B>>)(((Chain)(this.<Tuple<A, B>>addLink(((ChainLink<A, Tuple<A, B>>)(((ChainLink)(new MulLink<A, B>(by))))))))));
    }
    public <K> Chain<Tuple<K, ImArray<A>>> groupFactorBy(final double factor, final F<A, K> by) {
        return ((Chain<Tuple<K, ImArray<A>>>)(((Chain)(this.<Tuple<K, ImArray<A>>>addLink(((ChainLink<A, Tuple<K, ImArray<A>>>)(((ChainLink)(new MGroupByLink<A, K, ArrayBuilder<A>, ImArray<A>>(factor, by, new F0<ArrayBuilder<A>>() {
            @Override
            public ArrayBuilder<A> apply() {
                return ArrayBuilder.<A>apply();
            }
        }, new P2<ArrayBuilder<A>, A>() {
            @Override
            public void apply(final ArrayBuilder<A> b, final A item) {
                b.appendItem(item);
            }
        }, ((F<ArrayBuilder<A>, ImArray<A>>)(((F)(new F<ArrayBuilder<A>, ImArray<A>>() {
            @Override
            public ImArray<A> apply(final ArrayBuilder<A> b) {
                return b.build();
            }
        }))))))))))))));
    }
    public <K> Chain<Tuple<K, ImArray<A>>> groupBy(final F<A, K> by) {
        return this.<K>groupFactorBy(0.5, by);
    }
    public <K, B> Chain<Tuple<K, ImArray<B>>> groupFactorByF(final double factor, final F<A, K> by, final F<A, B> f) {
        return ((Chain<Tuple<K, ImArray<B>>>)(((Chain)(this.<Tuple<K, ImArray<B>>>addLink(((ChainLink<A, Tuple<K, ImArray<B>>>)(((ChainLink)(new MGroupByLink<A, K, ArrayBuilder<B>, ImArray<B>>(factor, by, new F0<ArrayBuilder<B>>() {
            @Override
            public ArrayBuilder<B> apply() {
                return ArrayBuilder.<B>apply();
            }
        }, new P2<ArrayBuilder<B>, A>() {
            @Override
            public void apply(final ArrayBuilder<B> b, final A item) {
                b.appendItem(f.apply(item));
            }
        }, ((F<ArrayBuilder<B>, ImArray<B>>)(((F)(new F<ArrayBuilder<B>, ImArray<B>>() {
            @Override
            public ImArray<B> apply(final ArrayBuilder<B> b) {
                return b.build();
            }
        }))))))))))))));
    }
    public <K, B> Chain<Tuple<K, ImArray<B>>> groupByF(final F<A, K> by, final F<A, B> f) {
        return this.<K, B>groupFactorByF(0.5, by, f);
    }
    public <K, C extends Traversable<A>> Chain<Tuple<K, C>> groupFactorByBuilder(final double factor, final F<A, K> by, final F0<Builder<A, C>> builder) {
        return ((Chain<Tuple<K, C>>)(((Chain)(this.<Tuple<K, C>>addLink(((ChainLink<A, Tuple<K, C>>)(((ChainLink)(new MGroupByLink<A, K, Builder<A, C>, C>(factor, by, builder, new P2<Builder<A, C>, A>() {
            @Override
            public void apply(final Builder<A, C> b, final A item) {
                b.appendItem(item);
            }
        }, new F<Builder<A, C>, C>() {
            @Override
            public C apply(final Builder<A, C> b) {
                return b.build();
            }
        }))))))))));
    }
    public <K, C extends Traversable<A>> Chain<Tuple<K, C>> groupByBuilder(final F<A, K> by, final F0<Builder<A, C>> builder) {
        return this.<K, C>groupFactorByBuilder(0.5, by, builder);
    }
    public <B, K, C extends Traversable<B>> Chain<Tuple<K, C>> groupFactorByFBuilder(final double factor, final F<A, K> by, final F<A, B> f, final F0<Builder<B, C>> builder) {
        return ((Chain<Tuple<K, C>>)(((Chain)(this.<Tuple<K, C>>addLink(((ChainLink<A, Tuple<K, C>>)(((ChainLink)(new MGroupByLink<A, K, Builder<B, C>, C>(factor, by, builder, new P2<Builder<B, C>, A>() {
            @Override
            public void apply(final Builder<B, C> b, final A item) {
                b.appendItem(f.apply(item));
            }
        }, new F<Builder<B, C>, C>() {
            @Override
            public C apply(final Builder<B, C> b) {
                return b.build();
            }
        }))))))))));
    }
    public <B, K, C extends Traversable<B>> Chain<Tuple<K, C>> groupByFBuilder(final F<A, K> by, final F<A, B> f, final F0<Builder<B, C>> builder) {
        return this.<B, K, C>groupFactorByFBuilder(0.5, by, f, builder);
    }
    public <K, V> Chain<Tuple<K, V>> groupFactorByStartFold(final double factor, final F<A, K> by, final F0<V> start, final F2<V, A, V> fold) {
        return ((Chain<Tuple<K, V>>)(((Chain)(this.<Tuple<K, V>>addLink(((ChainLink<A, Tuple<K, V>>)(((ChainLink)(new ImGroupByLink<A, K, V>(factor, by, start, fold))))))))));
    }
    public <K, V> Chain<Tuple<K, V>> groupByStartFold(final F<A, K> by, final F0<V> start, final F2<V, A, V> fold) {
        return this.<K, V>groupFactorByStartFold(0.5, by, start, fold);
    }
    public Chain<A> distinctFactor(final double factor) {
        return this.<A>addLink(((ChainLink<A, A>)(((ChainLink)(new DistinctLink<A>(factor))))));
    }
    public Chain<A> distinct() {
        return distinctFactor(0.5);
    }
    public <B> Chain<Tuple<A, B>> zipB(final Iterable<B> b) {
        return this.<Tuple<A, B>>addLink(((ChainLink<A, Tuple<A, B>>)(((ChainLink)(new ZipLink<A, B, Tuple<A, B>>(b, ((F2<A, B, Tuple<A, B>>)(((F2)(new F2<A, B, Tuple<A, B>>() {
            @Override
            public Tuple<A, B> apply(final A aa, final B bb) {
                return new Tuple<A, B>(aa, bb);
            }
        }))))))))));
    }
    public <B, X> Chain<X> zipBBy(final Iterable<B> b, final F2<A, B, X> by) {
        return this.<X>addLink(((ChainLink<A, X>)(((ChainLink)(new ZipLink<A, B, X>(b, by))))));
    }
    public <B> void zipForBBy(final Iterable<B> b, final P2<A, B> by) {
        final Iterator<B> bi = b.iterator();
        applyYield(Yield.<A>makeYield(new F<A, Go>() {
            @Override
            public Go apply(final A a) {
                if(bi.hasNext()) {
                    by.apply(a, bi.next());
                    return Go.Continue;
                } else {
                    return Go.Break;
                }
            }
        }));
    }
    public <B, C> Chain<Tuple3<A, B, C>> zip3BC(final Iterable<B> b, final Iterable<C> c) {
        return this.<Tuple3<A, B, C>>addLink(((ChainLink<A, Tuple3<A, B, C>>)(((ChainLink)(new Zip3Link<A, B, C, Tuple3<A, B, C>>(b, c, ((F3<A, B, C, Tuple3<A, B, C>>)(((F3)(new F3<A, B, C, Tuple3<A, B, C>>() {
            @Override
            public Tuple3<A, B, C> apply(final A aa, final B bb, final C cc) {
                return new Tuple3<A, B, C>(aa, bb, cc);
            }
        }))))))))));
    }
    public <B, C, X> Chain<X> zip3BCBy(final Iterable<B> b, final Iterable<C> c, final F3<A, B, C, X> by) {
        return this.<X>addLink(((ChainLink<A, X>)(((ChainLink)(new Zip3Link<A, B, C, X>(b, c, by))))));
    }
    public Chain<A> prependCollection(final Traversable<A> collection) {
        return this.<A>addLink(((ChainLink<A, A>)(((ChainLink)(new PrependLink<A>(collection))))));
    }
    public Chain<A> appendCollection(final Traversable<A> collection) {
        return this.<A>addLink(((ChainLink<A, A>)(((ChainLink)(new AppendLink<A>(collection))))));
    }
    public Chain<A> excludeCollection(final Traversable<A> collection) {
        final Traversable<A> c = ((collection instanceof Chain) ? (((Traversable<A>)(((Traversable)(((Chain<A>)(((Chain)(collection)))).toSet()))))) : (collection));
        return filterWhen(new F<A, Boolean>() {
            @Override
            public Boolean apply(final A item) {
                return !(c.containsItem(item));
            }
        });
    }
    public Chain<A> intersectCollection(final Iterable<A> collection) {
        final Traversable<A> c = ((collection instanceof Chain) ? (((Traversable<A>)(((Traversable)(((Chain<A>)(((Chain)(collection)))).toSet()))))) : (collection));
        return filterWhen(new F<A, Boolean>() {
            @Override
            public Boolean apply(final A item) {
                return c.containsItem(item);
            }
        });
    }
    public Chain<A> reverse() {
        return this.<A>addLink(((ChainLink<A, A>)(((ChainLink)(new ReverseLink<A>())))));
    }
    public Chain<A> reverseWhen(final boolean when) {
        if(when) {
            return ((Chain<A>)(((Chain)(this.<A>addLink(((ChainLink<A, A>)(((ChainLink)(new ReverseLink<A>())))))))));
        } else {
            return this;
        }
    }
    public <B extends Comparable<B>> Chain<B> sort() {
        return ((Chain<B>)(((Chain)(this)))).<B>addLink(((ChainLink<B, B>)(((ChainLink)(new SortLink<B>(new F2<B, B, Integer>() {
            @Override
            public Integer apply(final B a, final B b) {
                return a.compareTo(b);
            }
        }))))));
    }
    public <B extends Comparable<B>> Chain<B> sortDesc() {
        return ((Chain<B>)(((Chain)(this)))).<B>addLink(((ChainLink<B, B>)(((ChainLink)(new SortLink<B>(new F2<B, B, Integer>() {
            @Override
            public Integer apply(final B a, final B b) {
                return -(a.compareTo(b));
            }
        }))))));
    }
    public Chain<A> sortComparator(final F2<A, A, Integer> comparator) {
        return this.<A>addLink(((ChainLink<A, A>)(((ChainLink)(new SortLink<A>(comparator))))));
    }
    public SortBuilder<A> sortBy() {
        return new SortBuilder<A>(this);
    }
    public Chain<A> shuffle() {
        return this.<A>addLink(((ChainLink<A, A>)(((ChainLink)(new ShuffleLink<A>())))));
    }
    @Override
    public Go goOn(final F<A, Go> on) {
        return applyYield(Yield.<A>makeYield(on));
    }
    public <B> B foldStartBy(final B start, final F2<B, A, B> by) {
        final Mut<B> r = new Mut<B>(start);
        applyYield(Yield.<A>makeYield(new F<A, Go>() {
            @Override
            public Go apply(final A item) {
                r.value = by.apply(r.value, item);
                return Go.Continue;
            }
        }));
        return r.value;
    }
    public int count() {
        final Mut<Integer> r = new Mut<Integer>(0);
        _forEach(new P<A>() {
            @Override
            public void apply(final A _) {
                r.value++;
            }
        });
        return ((int)(r.value));
    }
    private void _forEach(final P<A> each) {
        applyYield(Yield.<A>makeYield(new F<A, Go>() {
            @Override
            public Go apply(final A item) {
                each.apply(item);
                return Go.Continue;
            }
        }));
    }
    public A last() {
        final Mut<A> ret = new Mut<A>();
        _forEach(new P<A>() {
            @Override
            public void apply(final A item) {
                ret.value = item;
            }
        });
        return ret.value;
    }
    public A randomItem() {
        final Seq<A> s = this.toSeq();
        final int n = s.count();
        if(n == 0) {
            return null;
        } else {
            return s.applyIndex(UInt.rndMax(n - 1));
        }
    }
    public A randomItemSeed(final Seed seed) {
        final Seq<A> s = this.toSeq();
        final int n = s.count();
        if(n == 0) {
            return null;
        } else {
            return s.applyIndex(((int)(seed.nextIntMinMax(((int)(0)), ((int)(n - 1))))));
        }
    }
    public boolean isEmpty() {
        final Mut<Boolean> ret = new Mut<Boolean>(true);
        applyYield(Yield.<A>makeYield(new F<A, Go>() {
            @Override
            public Go apply(final A item) {
                ret.value = true;
                return Go.Break;
            }
        }));
        return ret.value;
    }
    public <B extends Comparable<B>> Tuple<B, B> gap() {
        final Mut<B> min = new Mut<B>();
        final Mut<B> max = new Mut<B>();
        ((Chain<B>)(((Chain)(this))))._forEach(new P<B>() {
            @Override
            public void apply(final B item) {
                if(min.value == null || min.value.compareTo(item) > 0) {
                    min.value = item;
                }
                if(max.value == null || max.value.compareTo(item) < 0) {
                    max.value = item;
                }
            }
        });
        if(min.value == null) {
            return null;
        } else {
            if(max.value == null) {
                throw new NullPointerException();
            }
            return new Tuple<B, B>(min.value, max.value);
        }
    }
    public <B extends Comparable<B>> B min() {
        final Mut<B> min = new Mut<B>();
        ((Chain<B>)(((Chain)(this))))._forEach(new P<B>() {
            @Override
            public void apply(final B item) {
                if(min.value == null || min.value.compareTo(item) > 0) {
                    min.value = item;
                }
            }
        });
        return min.value;
    }
    public <B extends Comparable<B>> B max() {
        final Mut<B> max = new Mut<B>();
        ((Chain<B>)(((Chain)(this))))._forEach(new P<B>() {
            @Override
            public void apply(final B item) {
                if(max.value == null || max.value.compareTo(item) < 0) {
                    max.value = item;
                }
            }
        });
        return max.value;
    }
    public boolean or() {
        final Mut<Boolean> ret = new Mut<Boolean>(false);
        ((Chain<Boolean>)(((Chain)(this)))).applyYield(Yield.<Boolean>makeYield(new F<Boolean, Go>() {
            @Override
            public Go apply(final Boolean item) {
                if(item) {
                    ret.value = true;
                    return Go.Break;
                } else {
                    return Go.Continue;
                }
            }
        }));
        return ret.value;
    }
    public boolean and() {
        final Mut<Boolean> ret = new Mut<Boolean>(true);
        ((Chain<Boolean>)(((Chain)(this)))).applyYield(Yield.<Boolean>makeYield(new F<Boolean, Go>() {
            @Override
            public Go apply(final Boolean item) {
                if(!(item)) {
                    ret.value = false;
                    return Go.Break;
                } else {
                    return Go.Continue;
                }
            }
        }));
        return ret.value;
    }
    public Seq<A> toSeq() {
        final Mut<Builder<A, Seq<A>>> __il_b = new Mut<Builder<A, Seq<A>>>();
        final Mut<Seq<A>> __il_r = new Mut<Seq<A>>();
        applyYield(Yield.<A>makeBeginYieldAll(new F<Integer, Go>() {
            @Override
            public Go apply(final Integer size) {
                final int _ = ((int)(size));
                __il_b.value = ((Builder<A, Seq<A>>)(((Builder)(new ArrayBuilder<A>(((int)(_)))))));
                return Go.Continue;
            }
        }, new F<A, Go>() {
            @Override
            public Go apply(final A item) {
                if(__il_b.value == null) {
                    throw new NullPointerException();
                }
                __il_b.value.appendItem(item);
                return Go.Continue;
            }
        }, new F2<Yield<A>, Traversable<A>, Go>() {
            @Override
            public Go apply(final Yield<A> yield, final Traversable<A> all) {
                if(all instanceof Seq) {
                    __il_r.value = ((Seq<A>)(((Seq)(all))));
                    return Go.Continue;
                } else {
                    return yield.stdYieldAllItems(all);
                }
            }
        }));
        if(__il_r.value == null) {
            if(__il_b.value == null) {
                throw new NullPointerException();
            }
            return ((Seq<A>)(((Seq)(__il_b.value.build()))));
        } else {
            return ((Seq<A>)(((Seq)(__il_r.value))));
        }
    }
    public ImArray<A> toArray() {
        final Mut<ArrayBuilder<A>> b = new Mut<ArrayBuilder<A>>();
        final Mut<ImArray<A>> r = new Mut<ImArray<A>>();
        applyYield(Yield.<A>makeBeginYieldAll(new F<Integer, Go>() {
            @Override
            public Go apply(final Integer size) {
                b.value = new ArrayBuilder<A>(size);
                return Go.Continue;
            }
        }, new F<A, Go>() {
            @Override
            public Go apply(final A item) {
                if(b.value == null) {
                    throw new NullPointerException();
                }
                b.value.appendItem(item);
                return Go.Continue;
            }
        }, new F2<Yield<A>, Traversable<A>, Go>() {
            @Override
            public Go apply(final Yield<A> yield, final Traversable<A> all) {
                if(all instanceof ImArray) {
                    r.value = ((ImArray<A>)(((ImArray)(all))));
                    return Go.Continue;
                } else {
                    if(all instanceof MArray) {
                        r.value = ((MArray<A>)(((MArray)(all)))).im();
                        return Go.Continue;
                    } else {
                        return yield.stdYieldAllItems(all);
                    }
                }
            }
        }));
        if(r.value == null) {
            if(b.value == null) {
                throw new NullPointerException();
            }
            return b.value.build();
        } else {
            return r.value;
        }
    }
    public ImList<A> toList() {
        final Mut<Builder<A, ImList<A>>> __il_b = new Mut<Builder<A, ImList<A>>>();
        final Mut<ImList<A>> __il_r = new Mut<ImList<A>>();
        applyYield(Yield.<A>makeBeginYieldAll(new F<Integer, Go>() {
            @Override
            public Go apply(final Integer size) {
                final int _ = ((int)(size));
                __il_b.value = ((Builder<A, ImList<A>>)(((Builder)(new ImListBuilder<A>()))));
                return Go.Continue;
            }
        }, new F<A, Go>() {
            @Override
            public Go apply(final A item) {
                if(__il_b.value == null) {
                    throw new NullPointerException();
                }
                __il_b.value.appendItem(item);
                return Go.Continue;
            }
        }, new F2<Yield<A>, Traversable<A>, Go>() {
            @Override
            public Go apply(final Yield<A> yield, final Traversable<A> all) {
                if(all instanceof ImList) {
                    __il_r.value = ((ImList<A>)(((ImList)(all))));
                    return Go.Continue;
                } else {
                    return yield.stdYieldAllItems(all);
                }
            }
        }));
        if(__il_r.value == null) {
            if(__il_b.value == null) {
                throw new NullPointerException();
            }
            return ((ImList<A>)(((ImList)(__il_b.value.build()))));
        } else {
            return ((ImList<A>)(((ImList)(__il_r.value))));
        }
    }
    public Set<A> toSet() {
        final Mut<Builder<A, Set<A>>> __il_b = new Mut<Builder<A, Set<A>>>();
        final Mut<Set<A>> __il_r = new Mut<Set<A>>();
        applyYield(Yield.<A>makeBeginYieldAll(new F<Integer, Go>() {
            @Override
            public Go apply(final Integer size) {
                final int _ = ((int)(size));
                __il_b.value = ((Builder<A, Set<A>>)(((Builder)(new HashSetBuilder<A>(((int)(_)))))));
                return Go.Continue;
            }
        }, new F<A, Go>() {
            @Override
            public Go apply(final A item) {
                if(__il_b.value == null) {
                    throw new NullPointerException();
                }
                __il_b.value.appendItem(item);
                return Go.Continue;
            }
        }, new F2<Yield<A>, Traversable<A>, Go>() {
            @Override
            public Go apply(final Yield<A> yield, final Traversable<A> all) {
                if(all instanceof Set) {
                    __il_r.value = ((Set<A>)(((Set)(all))));
                    return Go.Continue;
                } else {
                    return yield.stdYieldAllItems(all);
                }
            }
        }));
        if(__il_r.value == null) {
            if(__il_b.value == null) {
                throw new NullPointerException();
            }
            return ((Set<A>)(((Set)(__il_b.value.build()))));
        } else {
            return ((Set<A>)(((Set)(__il_r.value))));
        }
    }
    public <B extends Comparable<B>> TreeSet<B> toTreeSet() {
        final Mut<Builder<B, ImTreeSet<B>>> b = new Mut<Builder<B, ImTreeSet<B>>>();
        final Mut<TreeSet<B>> r = new Mut<TreeSet<B>>();
        ((Chain<B>)(((Chain)(this)))).applyYield(Yield.<B>makeBeginYieldAll(new F<Integer, Go>() {
            @Override
            public Go apply(final Integer size) {
                b.value = ((Builder<B, ImTreeSet<B>>)(((Builder)(TreeSetBuilder.<B>apply()))));
                return Go.Continue;
            }
        }, new F<B, Go>() {
            @Override
            public Go apply(final B item) {
                if(b.value == null) {
                    throw new NullPointerException();
                }
                b.value.appendItem(item);
                return Go.Continue;
            }
        }, new F2<Yield<B>, Traversable<B>, Go>() {
            @Override
            public Go apply(final Yield<B> yield, final Traversable<B> all) {
                if(all instanceof TreeSet) {
                    r.value = ((TreeSet<B>)(((TreeSet)(all))));
                    return Go.Continue;
                } else {
                    return yield.stdYieldAllItems(all);
                }
            }
        }));
        if(r.value == null) {
            if(b.value == null) {
                throw new NullPointerException();
            }
            return ((TreeSet<B>)(((TreeSet)(b.value.build()))));
        } else {
            return ((TreeSet<B>)(((TreeSet)(r.value))));
        }
    }
    public <K, V> ImHashMap<K, V> toMap() {
        final Mut<Builder<Tuple<K, V>, ImHashMap<K, V>>> b = new Mut<Builder<Tuple<K, V>, ImHashMap<K, V>>>();
        final Mut<ImHashMap<K, V>> r = new Mut<ImHashMap<K, V>>();
        ((Chain<Tuple<K, V>>)(((Chain)(this)))).applyYield(Yield.<Tuple<K, V>>makeBeginYieldAll(new F<Integer, Go>() {
            @Override
            public Go apply(final Integer size) {
                b.value = ((Builder<Tuple<K, V>, ImHashMap<K, V>>)(((Builder)(new HashMapBuilder<K, V>()))));
                return Go.Continue;
            }
        }, new F<Tuple<K, V>, Go>() {
            @Override
            public Go apply(final Tuple<K, V> item) {
                if(b.value == null) {
                    throw new NullPointerException();
                }
                b.value.appendItem(item);
                return Go.Continue;
            }
        }, new F2<Yield<Tuple<K, V>>, Traversable<Tuple<K, V>>, Go>() {
            @Override
            public Go apply(final Yield<Tuple<K, V>> yield, final Traversable<Tuple<K, V>> all) {
                if(all instanceof ImHashMap) {
                    r.value = ((ImHashMap<K, V>)(((ImHashMap)(all))));
                    return Go.Continue;
                } else {
                    if(all instanceof MHashMap) {
                        r.value = ((MHashMap<K, V>)(((MHashMap)(all)))).im();
                        return Go.Continue;
                    } else {
                        return yield.stdYieldAllItems(all);
                    }
                }
            }
        }));
        if(r.value == null) {
            if(b.value == null) {
                throw new NullPointerException();
            }
            return b.value.build();
        } else {
            return r.value;
        }
    }
    public String toStringStartDelimiterEnd(final String start, final String delimiter, final String end) {
        final StringBuilder b = new StringBuilder();
        b.appendStr(start);
        final Mut<Boolean> first = new Mut<Boolean>(true);
        _forEach(new P<A>() {
            @Override
            public void apply(final A item) {
                if(first.value) {
                    first.value = false;
                } else {
                    b.appendStr(delimiter);
                }
                b.appendObj(item);
            }
        });
        b.appendStr(end);
        return b.toString();
    }
    public String toStringDelimiter(final String delimiter) {
        return toStringStartDelimiterEnd("", delimiter, "");
    }
    public <V, R> Future<R> futureF(final F<Chain<V>, R> f) {
        final FutureEnd<V> lnk = new FutureEnd<V>();
        ((Chain<Future<V>>)(((Chain)(this)))).applyYield(lnk.yield());
        return lnk.future().<R>mapF(new F<ImArray<V>, R>() {
            @Override
            public R apply(final ImArray<V> o) {
                return f.apply(o.chain());
            }
        });
    }
    public <V> Future<ImArray<V>> future() {
        final FutureEnd<V> lnk = new FutureEnd<V>();
        ((Chain<Future<V>>)(((Chain)(this)))).applyYield(lnk.yield());
        return ((Future<ImArray<V>>)(((Future)(lnk.future()))));
    }
    public Future<Void> voidFuture() {
        final FutureVoidEnd lnk = new FutureVoidEnd();
        ((Chain<Future<Void>>)(((Chain)(this)))).applyYield(lnk.yield());
        return lnk.future();
    }
    public Go applyYield(final Yield<A> yield) {
        final Yield<?> y = buildYield(yield);
        final Go r = y.beginYieldWithSize(((int)(0)));
        return y.endYieldWithResult(r);
    }
    private Yield<?> buildYield(final Yield<A> yield) {
        Chain<?> ch = this;
        Yield<?> y = yield;
        while(ch != null) {
            y = ((Yield)(ch.link.buildYield(((Yield)(y)))));
            ch = ((Chain)(ch.previous));
        }
        return ((Yield)(y));
    }
    public <B> Chain<B> addLink(final ChainLink<A, B> link) {
        return new Chain<B>(link, this);
    }
    public static <T> Traversable<T> resolveCollection(final Traversable<T> collection) {
        if(collection instanceof Chain) {
            return ((Traversable<T>)(((Traversable)(((Chain<T>)(((Chain)(collection)))).toArray()))));
        } else {
            return collection;
        }
    }
    public static <T> Traversable<T> resolveToSetCollection(final Traversable<T> collection) {
        if(collection instanceof Chain) {
            return ((Traversable<T>)(((Traversable)(((Chain<T>)(((Chain)(collection)))).toSet()))));
        } else {
            return collection;
        }
    }
    public Chain(final ChainLink<?, A> link, final Chain<?> previous) {
        this.link = link;
        this.previous = previous;
    }
    public String toString() {
        return String.format("Chain(%s, %s)", this.link, this.previous);
    }
}