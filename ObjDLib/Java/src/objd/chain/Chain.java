package objd.chain;

import objd.lang.*;
import objd.collection.Traversable;
import objd.collection.ArrayBuilder;
import objd.collection.ImArray;
import objd.collection.Builder;
import objd.collection.Iterable;
import objd.collection.Iterator;
import objd.collection.Go;
import objd.collection.Seq;
import objd.collection.MArray;
import objd.collection.ImList;
import objd.collection.Set;
import objd.collection.TreeSet;
import objd.collection.TreeSetBuilder;
import objd.collection.ImHashMap;
import objd.collection.StringBuilder;
import objd.collection.String;
import objd.concurrent.Future;
import objd.collection.ImTraversable_impl;

public class Chain<A> extends ImTraversable_impl<A> {
    public final ChainLink<?, A> link;
    public final Chain<?> previous;
    public static <T> Chain<T> applyCollection(final Traversable<T> collection) {
        return new Chain<T>(new SourceLink<T>(collection), null);
    }
    public Chain<A> filterFactorWhen(final double factor, final F<A, Boolean> when) {
        return this.<A>addLink(new FilterLink<A>(factor, when));
    }
    public Chain<A> filterWhen(final F<A, Boolean> when) {
        return filterFactorWhen(0.5, when);
    }
    public Chain<A> topNumbers(final int numbers) {
        return this.<A>addLink(new TopLink<A>(((int)(numbers))));
    }
    public <B> Chain<B> filterCastFactorTo(final double factor, final ClassType<B> to) {
        return ((Chain<B>)(this.<A>addLink(new FilterLink<A>(factor, new F<A, Boolean>() {
            @Override
            public Boolean apply(final A item) {
                return to.isInstanceObj(item);
            }
        }))));
    }
    public <B> Chain<B> filterCastTo(final ClassType<B> to) {
        return this.<B>filterCastFactorTo(0.5, to);
    }
    public <B> Chain<B> mapF(final F<A, B> f) {
        return this.<B>addLink(new MapLink<A, B>(f));
    }
    public <B> Chain<B> mapOptF(final F<A, B> f) {
        return this.<B>addLink(new MapOptLink<A, B>(f));
    }
    public <B> Chain<B> flatMapFactorF(final double factor, final F<A, Traversable<B>> f) {
        return this.<B>addLink(new FlatMapLink<A, B>(factor, f));
    }
    public <B> Chain<B> flatMapF(final F<A, Traversable<B>> f) {
        return this.<B>flatMapFactorF(2.0, f);
    }
    public <B> Chain<B> flatFactor(final double factor) {
        return ((Chain<Traversable<B>>)(this)).<B>addLink(new FlatLink<B>(factor));
    }
    public <B> Chain<B> flat() {
        return this.<B>flatFactor(2.0);
    }
    public Chain<Tuple<A, A>> combinations() {
        return this.<Tuple<A, A>>addLink(new CombinationsLink<A>());
    }
    public <B> Chain<B> uncombinations() {
        return ((Chain<Tuple<B, B>>)(this)).<B>addLink(new UncombinationsLink<B>());
    }
    public Chain<Tuple<A, A>> neighbours() {
        return this.<Tuple<A, A>>addLink(new NeighboursLink<A>(false));
    }
    public Chain<Tuple<A, A>> neighboursRing() {
        return this.<Tuple<A, A>>addLink(new NeighboursLink<A>(true));
    }
    public <B> Chain<Tuple<A, B>> mulBy(final Traversable<B> by) {
        return this.<Tuple<A, B>>addLink(new MulLink<A, B>(by));
    }
    public <K> Chain<Tuple<K, ImArray<A>>> groupFactorBy(final double factor, final F<A, K> by) {
        return this.<Tuple<K, ImArray<A>>>addLink(new MGroupByLink<A, K, ArrayBuilder<A>, ImArray<A>>(factor, by, new F0<ArrayBuilder<A>>() {
            @Override
            public ArrayBuilder<A> apply() {
                return ArrayBuilder.<A>apply();
            }
        }, new P2<ArrayBuilder<A>, A>() {
            @Override
            public void apply(final ArrayBuilder<A> b, final A item) {
                b.appendItem(item);
            }
        }, new F<ArrayBuilder<A>, ImArray<A>>() {
            @Override
            public ImArray<A> apply(final ArrayBuilder<A> b) {
                return b.build();
            }
        }));
    }
    public <K> Chain<Tuple<K, ImArray<A>>> groupBy(final F<A, K> by) {
        return this.<K>groupFactorBy(0.5, by);
    }
    public <K, B> Chain<Tuple<K, ImArray<B>>> groupFactorByF(final double factor, final F<A, K> by, final F<A, B> f) {
        return this.<Tuple<K, ImArray<B>>>addLink(new MGroupByLink<A, K, ArrayBuilder<B>, ImArray<B>>(factor, by, new F0<ArrayBuilder<B>>() {
            @Override
            public ArrayBuilder<B> apply() {
                return ArrayBuilder.<B>apply();
            }
        }, new P2<ArrayBuilder<B>, A>() {
            @Override
            public void apply(final ArrayBuilder<B> b, final A item) {
                b.appendItem(f.apply(item));
            }
        }, new F<ArrayBuilder<B>, ImArray<B>>() {
            @Override
            public ImArray<B> apply(final ArrayBuilder<B> b) {
                return b.build();
            }
        }));
    }
    public <K, B> Chain<Tuple<K, ImArray<B>>> groupByF(final F<A, K> by, final F<A, B> f) {
        return this.<K, B>groupFactorByF(0.5, by, f);
    }
    public <K, C extends Traversable<A>> Chain<Tuple<K, C>> groupFactorByBuilder(final double factor, final F<A, K> by, final F0<Builder<A, C>> builder) {
        return this.<Tuple<K, C>>addLink(new MGroupByLink<A, K, Builder<A, C>, C>(factor, by, builder, new P2<Builder<A, C>, A>() {
            @Override
            public void apply(final Builder<A, C> b, final A item) {
                b.appendItem(item);
            }
        }, new F<Builder<A, C>, C>() {
            @Override
            public C apply(final Builder<A, C> b) {
                return b.build();
            }
        }));
    }
    public <K, C extends Traversable<A>> Chain<Tuple<K, C>> groupByBuilder(final F<A, K> by, final F0<Builder<A, C>> builder) {
        return this.<K, C>groupFactorByBuilder(0.5, by, builder);
    }
    public <B, K, C extends Traversable<B>> Chain<Tuple<K, C>> groupFactorByFBuilder(final double factor, final F<A, K> by, final F<A, B> f, final F0<Builder<B, C>> builder) {
        return this.<Tuple<K, C>>addLink(new MGroupByLink<A, K, Builder<B, C>, C>(factor, by, builder, new P2<Builder<B, C>, A>() {
            @Override
            public void apply(final Builder<B, C> b, final A item) {
                b.appendItem(f.apply(item));
            }
        }, new F<Builder<B, C>, C>() {
            @Override
            public C apply(final Builder<B, C> b) {
                return b.build();
            }
        }));
    }
    public <B, K, C extends Traversable<B>> Chain<Tuple<K, C>> groupByFBuilder(final F<A, K> by, final F<A, B> f, final F0<Builder<B, C>> builder) {
        return this.<B, K, C>groupFactorByFBuilder(0.5, by, f, builder);
    }
    public <K, V> Chain<Tuple<K, V>> groupFactorByStartFold(final double factor, final F<A, K> by, final F0<V> start, final F2<V, A, V> fold) {
        return this.<Tuple<K, V>>addLink(new ImGroupByLink<A, K, V>(factor, by, start, fold));
    }
    public <K, V> Chain<Tuple<K, V>> groupByStartFold(final F<A, K> by, final F0<V> start, final F2<V, A, V> fold) {
        return this.<K, V>groupFactorByStartFold(0.5, by, start, fold);
    }
    public Chain<A> distinctFactor(final double factor) {
        return this.<A>addLink(new DistinctLink<A>(factor));
    }
    public Chain<A> distinct() {
        return distinctFactor(0.5);
    }
    public <B> Chain<Tuple<A, B>> zipB(final Iterable<B> b) {
        return this.<Tuple<A, B>>addLink(new ZipLink<A, B, Tuple<A, B>>(b, new F2<A, B, Tuple<A, B>>() {
            @Override
            public Tuple<A, B> apply(final A aa, final B bb) {
                return new Tuple<A, B>(aa, bb);
            }
        }));
    }
    public <B, X> Chain<X> zipBBy(final Iterable<B> b, final F2<A, B, X> by) {
        return this.<X>addLink(new ZipLink<A, B, X>(b, by));
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
        return this.<Tuple3<A, B, C>>addLink(new Zip3Link<A, B, C, Tuple3<A, B, C>>(b, c, new F3<A, B, C, Tuple3<A, B, C>>() {
            @Override
            public Tuple3<A, B, C> apply(final A aa, final B bb, final C cc) {
                return new Tuple3<A, B, C>(aa, bb, cc);
            }
        }));
    }
    public <B, C, X> Chain<X> zip3BCBy(final Iterable<B> b, final Iterable<C> c, final F3<A, B, C, X> by) {
        return this.<X>addLink(new Zip3Link<A, B, C, X>(b, c, by));
    }
    public Chain<A> prependCollection(final Traversable<A> collection) {
        return this.<A>addLink(new PrependLink<A>(collection));
    }
    public Chain<A> appendCollection(final Traversable<A> collection) {
        return this.<A>addLink(new AppendLink<A>(collection));
    }
    public Chain<A> excludeCollection(final Traversable<A> collection) {
        final Traversable<A> c = ((collection instanceof Chain) ? (((Traversable<A>)(((Chain<A>)(collection)).toSet()))) : (collection));
        return filterWhen(new F<A, Boolean>() {
            @Override
            public Boolean apply(final A item) {
                return !(c.containsItem(item));
            }
        });
    }
    public Chain<A> intersectCollection(final Iterable<A> collection) {
        final Traversable<A> c = ((collection instanceof Chain) ? (((Traversable<A>)(((Chain<A>)(collection)).toSet()))) : (collection));
        return filterWhen(new F<A, Boolean>() {
            @Override
            public Boolean apply(final A item) {
                return c.containsItem(item);
            }
        });
    }
    public Chain<A> reverse() {
        return this.<A>addLink(new ReverseLink<A>());
    }
    public Chain<A> reverseWhen(final boolean when) {
        if(when) {
            return ((Chain<A>)(this.<A>addLink(new ReverseLink<A>())));
        } else {
            return this;
        }
    }
    public <B extends Comparable<B>> Chain<A> sort() {
        return ((Chain<B>)(this)).<B>addLink(new SortLink<B>(new F2<B, B, Integer>() {
            @Override
            public Integer apply(final B a, final B b) {
                return a.compareTo(b);
            }
        }));
    }
    public <B extends Comparable<B>> Chain<A> sortDesc() {
        return ((Chain<B>)(this)).<B>addLink(new SortLink<B>(new F2<B, B, Integer>() {
            @Override
            public Integer apply(final B a, final B b) {
                return -(a.compareTo(b));
            }
        }));
    }
    public Chain<A> sortComparator(final F2<A, A, Integer> comparator) {
        return this.<A>addLink(new SortLink<A>(comparator));
    }
    public SortBuilder<A> sortBy() {
        return new SortBuilder<A>(this);
    }
    public Chain<A> shuffle() {
        return this.<A>addLink(new ShuffleLink<A>());
    }
    @Override
    public Go goOn(final F<A, Go> on) {
        return applyYield(Yield.<A>makeYield(on));
    }
    public <B> B foldStartBy(final B start, final F2<B, A, B> by) {
        final Mut<B> r = new Mut<B>(start);
        applyYield(Yield.<B>makeYield(new F<B, Go>() {
            @Override
            public Go apply(final B item) {
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
        ((Chain<B>)(this))._forEach(new P<B>() {
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
        ((Chain<B>)(this))._forEach(new P<B>() {
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
        ((Chain<B>)(this))._forEach(new P<B>() {
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
        ((Chain<Boolean>)(this)).applyYield(Yield.<Boolean>makeYield(new F<Boolean, Go>() {
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
        ((Chain<Boolean>)(this)).applyYield(Yield.<Boolean>makeYield(new F<Boolean, Go>() {
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
    private <R> R convertToBuilder(final Type<R> to, final F<Integer, Builder<A, R>> builder) {
        final Mut<Builder<A, R>> b = new Mut<Builder<A, R>>();
        final Mut<R> r = new Mut<R>();
        applyYield(Yield.<A>makeBeginYieldAll(new F<Integer, Go>() {
            @Override
            public Go apply(final Integer size) {
                b.value = builder.apply(((int)(size)));
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
                if(all instanceof R) {
                    r.value = ((R)(all));
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
            return b.value.build();
        } else {
            return r.value;
        }
    }
    public Seq<A> toSeq() {
        final Mut<Builder<A, Seq<A>>> __il_b = new Mut<Builder<A, Seq<A>>>();
        final Mut<Seq<A>> __il_r = new Mut<Seq<A>>();
        applyYield(Yield.<A>makeBeginYieldAll(new F<Integer, Go>() {
            @Override
            public Go apply(final Integer size) {
                final int _ = ((int)(size));
                __il_b.value = new ArrayBuilder<A>(((int)(_)));
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
                    __il_r.value = ((Seq<A>)(all));
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
            return __il_b.value.build();
        } else {
            return __il_r.value;
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
                    r.value = ((ImArray<A>)(all));
                    return Go.Continue;
                } else {
                    if(all instanceof MArray) {
                        r.value = ((MArray<A>)(all)).im();
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
        final Mut<Builder<A, ImList<?>>> __il_b = new Mut<Builder<A, ImList<?>>>();
        final Mut<ImList<?>> __il_r = new Mut<ImList<?>>();
        applyYield(Yield.<A>makeBeginYieldAll(new F<Integer, Go>() {
            @Override
            public Go apply(final Integer size) {
                final int _ = ((int)(size));
                __il_b.value = new ImListBuilder<A>();
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
                    __il_r.value = ((ImList<?>)(all));
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
            return __il_b.value.build();
        } else {
            return __il_r.value;
        }
    }
    public Set<A> toSet() {
        final Mut<Builder<A, Set<?>>> __il_b = new Mut<Builder<A, Set<?>>>();
        final Mut<Set<?>> __il_r = new Mut<Set<?>>();
        applyYield(Yield.<A>makeBeginYieldAll(new F<Integer, Go>() {
            @Override
            public Go apply(final Integer size) {
                final int _ = ((int)(size));
                __il_b.value = new HashSetBuilder<A>(((int)(_)));
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
                    __il_r.value = ((Set<?>)(all));
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
            return __il_b.value.build();
        } else {
            return __il_r.value;
        }
    }
    public <B extends Comparable<B>> TreeSet<B> toTreeSet() {
        final Mut<Builder<B, TreeSet<?>>> __il_b = new Mut<Builder<B, TreeSet<?>>>();
        final Mut<TreeSet<?>> __il_r = new Mut<TreeSet<?>>();
        ((Chain<B>)(this)).applyYield(Yield.<B>makeBeginYieldAll(new F<Integer, Go>() {
            @Override
            public Go apply(final Integer size) {
                final int _ = ((int)(size));
                __il_b.value = TreeSetBuilder.<B>apply();
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
                if(all instanceof TreeSet) {
                    __il_r.value = ((TreeSet<?>)(all));
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
            return __il_b.value.build();
        } else {
            return __il_r.value;
        }
    }
    public <K, V> ImHashMap<K, V> toMap() {
        final Mut<Builder<Tuple<K, V>, ImHashMap<?, ?>>> __il_b = new Mut<Builder<Tuple<K, V>, ImHashMap<?, ?>>>();
        final Mut<ImHashMap<?, ?>> __il_r = new Mut<ImHashMap<?, ?>>();
        ((Chain<Tuple<K, V>>)(this)).applyYield(Yield.<Tuple<K, V>>makeBeginYieldAll(new F<Integer, Go>() {
            @Override
            public Go apply(final Integer size) {
                final int _ = ((int)(size));
                __il_b.value = new HashMapBuilder<K, V>();
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
                if(all instanceof ImHashMap) {
                    __il_r.value = ((ImHashMap<?, ?>)(all));
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
            return __il_b.value.build();
        } else {
            return __il_r.value;
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
        return b.build();
    }
    public String toStringDelimiter(final String delimiter) {
        return toStringStartDelimiterEnd("", delimiter, "");
    }
    public String toString() {
        final Mut<Builder<Character, String>> __il_b = new Mut<Builder<Character, String>>();
        final Mut<String> __il_r = new Mut<String>();
        ((Chain<Character>)(this)).applyYield(Yield.<Character>makeBeginYieldAll(new F<Integer, Go>() {
            @Override
            public Go apply(final Integer size) {
                final int _ = ((int)(size));
                __il_b.value = new StringBuilder();
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
                if(all instanceof String) {
                    __il_r.value = ((String)(all));
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
            return __il_b.value.build();
        } else {
            return __il_r.value;
        }
    }
    public <V, R> Future<R> futureF(final F<Chain<V>, R> f) {
        final FutureEnd<V> lnk = new FutureEnd<V>();
        ((Chain<Future<V>>)(this)).applyYield(lnk.yield());
        return lnk.future().<R>mapF(new F<ImArray<V>, R>() {
            @Override
            public R apply(final ImArray<V> o) {
                return f.apply(o.chain());
            }
        });
    }
    public <V> Future<ImArray<V>> future() {
        final FutureEnd<V> lnk = new FutureEnd<V>();
        ((Chain<Future<V>>)(this)).applyYield(lnk.yield());
        return lnk.future();
    }
    public Future<Void> voidFuture() {
        final FutureVoidEnd lnk = new FutureVoidEnd();
        ((Chain<Future<Void>>)(this)).applyYield(lnk.yield());
        return lnk.future();
    }
    public Go applyYield(final Yield<A> yield) {
        final Yield<?> y = buildYield(yield);
        final Go r = y.beginYieldWithSize(((int)(0)));
        return y.endYieldWithResult(r);
    }
    private Yield<?> buildYield(final Yield<A> yield) {
        Chain<?> ch = this;
        Yield<A> y = yield;
        while(ch != null) {
            y = ch.link.buildYield(y);
            ch = ch.previous;
        }
        return y;
    }
    public <B> Chain<B> addLink(final ChainLink<A, B> link) {
        return new Chain<B>(link, this);
    }
    public static <T> Traversable<T> resolveCollection(final Traversable<T> collection) {
        if(collection instanceof Chain) {
            return ((Traversable<T>)(((Chain<T>)(collection)).toArray()));
        } else {
            return collection;
        }
    }
    public static <T> Traversable<T> resolveToSetCollection(final Traversable<T> collection) {
        if(collection instanceof Chain) {
            return ((Traversable<T>)(((Chain<T>)(collection)).toSet()));
        } else {
            return collection;
        }
    }
    public Chain(final ChainLink<?, A> link, final Chain<?> previous) {
        this.link = link;
        this.previous = previous;
    }
}