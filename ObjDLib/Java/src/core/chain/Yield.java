package core.chain;

public class Yield<T> {
    public final F<Integer, Integer> begin;
    public final F<T, Integer> yield;
    public final F<Integer, Integer> end;
    public final F<Traversable<T>, Integer> all;
    public static byte Continue() {
        return ((byte)0);
    }
    public static byte Break() {
        return ((byte)1);
    }
    public static <T> Yield<T> makeBeginYieldEndAll(final F<Integer, Integer> begin, final F<T, Integer> yield, final F<Integer, Integer> end, final F<Traversable<T>, Integer> all) {
        return new Yield<T>(begin, yield, end, all);
    }
    public static <T> Yield<T> makeBeginYieldEnd(final F<Integer, Integer> begin, final F<T, Integer> yield, final F<Integer, Integer> end) {
        return makeBeginYieldEndAll(begin, yield, end, null);
    }
    public static <T> Yield<T> makeBeginYieldAll(final F<Integer, Integer> begin, final F<T, Integer> yield, final F<Traversable<T>, Integer> all) {
        return makeBeginYieldEndAll(begin, yield, null, all);
    }
    public static <T> Yield<T> makeBeginYield(final F<Integer, Integer> begin, final F<T, Integer> yield) {
        return makeBeginYieldEndAll(begin, yield, null, null);
    }
    public static <T> Yield<T> makeBeginEndAll(final F<Integer, Integer> begin, final F<Integer, Integer> end, final F<Traversable<T>, Integer> all) {
        return makeBeginYieldEndAll(begin, null, end, all);
    }
    public static <T> Yield<T> makeBeginEnd(final F<Integer, Integer> begin, final F<Integer, Integer> end) {
        return makeBeginYieldEndAll(begin, null, end, null);
    }
    public static <T> Yield<T> makeBeginAll(final F<Integer, Integer> begin, final F<Traversable<T>, Integer> all) {
        return makeBeginYieldEndAll(begin, null, null, all);
    }
    public static <T> Yield<T> makeBegin(final F<Integer, Integer> begin) {
        return makeBeginYieldEndAll(begin, null, null, null);
    }
    public static <T> Yield<T> makeYieldEndAll(final F<T, Integer> yield, final F<Integer, Integer> end, final F<Traversable<T>, Integer> all) {
        return makeBeginYieldEndAll(null, yield, end, all);
    }
    public static <T> Yield<T> makeYieldEnd(final F<T, Integer> yield, final F<Integer, Integer> end) {
        return makeBeginYieldEndAll(null, yield, end, null);
    }
    public static <T> Yield<T> makeYieldAll(final F<T, Integer> yield, final F<Traversable<T>, Integer> all) {
        return makeBeginYieldEndAll(null, yield, null, all);
    }
    public static <T> Yield<T> makeYield(final F<T, Integer> yield) {
        return makeBeginYieldEndAll(null, yield, null, null);
    }
    public static <T> Yield<T> makeEndAll(final F<Integer, Integer> end, final F<Traversable<T>, Integer> all) {
        return makeBeginYieldEndAll(null, null, end, all);
    }
    public static <T> Yield<T> makeEnd(final F<Integer, Integer> end) {
        return makeBeginYieldEndAll(null, null, end, null);
    }
    public static <T> Yield<T> makeAll(final F<Traversable<T>, Integer> all) {
        return makeBeginYieldEndAll(null, null, null, all);
    }
    public static <T> Yield<T> make() {
        return makeBeginYieldEndAll(null, null, null, null);
    }
    public static <T> Yield<T> decorateBaseBeginYieldEndAll(final Yield<?> base, final F<Integer, Integer> begin, final F<T, Integer> yield, final F<Integer, Integer> end, final F<Traversable<T>, Integer> all) {
        final F<Integer, Integer> __tmp = begin;
        final F<T, Integer> __tmp = yield;
        final F<Integer, Integer> __tmp = end;
        return new Yield<T>((__tmp != null) ? (__tmp) : (new F<Integer, Integer>() {
            @Override
            public Integer apply(final Integer size) {
                return base.beginYieldWithSize(size);
            }
        }), (__tmp != null) ? (__tmp) : (new F<T, Integer>() {
            @Override
            public Integer apply(final T item) {
                return base.yieldItem(item);
            }
        }), (__tmp != null) ? (__tmp) : (new F<Integer, Integer>() {
            @Override
            public Integer apply(final Integer result) {
                return base.endYieldWithResult(result);
            }
        }), all);
    }
    public static <T> Yield<T> decorateBaseBeginYieldEnd(final Yield<?> base, final F<Integer, Integer> begin, final F<T, Integer> yield, final F<Integer, Integer> end) {
        return decorateBaseBeginYieldEndAll(base, begin, yield, end, null);
    }
    public static <T> Yield<T> decorateBaseBeginYieldAll(final Yield<?> base, final F<Integer, Integer> begin, final F<T, Integer> yield, final F<Traversable<T>, Integer> all) {
        return decorateBaseBeginYieldEndAll(base, begin, yield, null, all);
    }
    public static <T> Yield<T> decorateBaseBeginYield(final Yield<?> base, final F<Integer, Integer> begin, final F<T, Integer> yield) {
        return decorateBaseBeginYieldEndAll(base, begin, yield, null, null);
    }
    public static <T> Yield<T> decorateBaseBeginEndAll(final Yield<?> base, final F<Integer, Integer> begin, final F<Integer, Integer> end, final F<Traversable<T>, Integer> all) {
        return decorateBaseBeginYieldEndAll(base, begin, null, end, all);
    }
    public static <T> Yield<T> decorateBaseBeginEnd(final Yield<?> base, final F<Integer, Integer> begin, final F<Integer, Integer> end) {
        return decorateBaseBeginYieldEndAll(base, begin, null, end, null);
    }
    public static <T> Yield<T> decorateBaseBeginAll(final Yield<?> base, final F<Integer, Integer> begin, final F<Traversable<T>, Integer> all) {
        return decorateBaseBeginYieldEndAll(base, begin, null, null, all);
    }
    public static <T> Yield<T> decorateBaseBegin(final Yield<?> base, final F<Integer, Integer> begin) {
        return decorateBaseBeginYieldEndAll(base, begin, null, null, null);
    }
    public static <T> Yield<T> decorateBaseYieldEndAll(final Yield<?> base, final F<T, Integer> yield, final F<Integer, Integer> end, final F<Traversable<T>, Integer> all) {
        return decorateBaseBeginYieldEndAll(base, null, yield, end, all);
    }
    public static <T> Yield<T> decorateBaseYieldEnd(final Yield<?> base, final F<T, Integer> yield, final F<Integer, Integer> end) {
        return decorateBaseBeginYieldEndAll(base, null, yield, end, null);
    }
    public static <T> Yield<T> decorateBaseYieldAll(final Yield<?> base, final F<T, Integer> yield, final F<Traversable<T>, Integer> all) {
        return decorateBaseBeginYieldEndAll(base, null, yield, null, all);
    }
    public static <T> Yield<T> decorateBaseYield(final Yield<?> base, final F<T, Integer> yield) {
        return decorateBaseBeginYieldEndAll(base, null, yield, null, null);
    }
    public static <T> Yield<T> decorateBaseEndAll(final Yield<?> base, final F<Integer, Integer> end, final F<Traversable<T>, Integer> all) {
        return decorateBaseBeginYieldEndAll(base, null, null, end, all);
    }
    public static <T> Yield<T> decorateBaseEnd(final Yield<?> base, final F<Integer, Integer> end) {
        return decorateBaseBeginYieldEndAll(base, null, null, end, null);
    }
    public static <T> Yield<T> decorateBaseAll(final Yield<?> base, final F<Traversable<T>, Integer> all) {
        return decorateBaseBeginYieldEndAll(base, null, null, null, all);
    }
    public static <T> Yield<T> decorateBase(final Yield<?> base) {
        return decorateBaseBeginYieldEndAll(base, null, null, null, null);
    }
    public int beginYieldWithSize(final int size) {
        if(this.begin == null) {
            return ((int)((byte)0));
        } else {
            return this.begin.apply(size);
        }
    }
    public int yieldItem(final T item) {
        if(this.yield == null) {
            return ((int)((byte)0));
        } else {
            return this.yield.apply(item);
        }
    }
    public int endYieldWithResult(final int result) {
        if(this.end == null) {
            return result;
        } else {
            return this.end.apply(result);
        }
    }
    public int yieldAllItems(final Traversable<T> items) {
        if(this.all != null) {
            return this.all.apply(items);
        } else {
            return stdYieldAllItems(items);
        }
    }
    public int stdYieldAllItems(final Traversable<T> items) {
        final Mut<Integer> result = new Mut<Integer>(((int)((byte)0)));
        if(items instanceof Array) {
            final Array<T> _items = ((Array<T>)items);
            if(beginYieldWithSize(_items.count()) == ((byte)1)) {
                return ((int)((byte)1));
            } else {
                _items.goOn(new F<T, Boolean>() {
                    @Override
                    public Boolean apply(final T item) {
                        result.value = ((int)yieldItem(item));
                        return result.value == ((byte)0);
                    }
                });
            }
        } else {
            if(items instanceof Iterable) {
                final Iterable<T> _items = ((Iterable<T>)items);
                if(beginYieldWithSize(_items.count()) == ((byte)1)) {
                    return ((int)((byte)1));
                } else {
                    items.goOn(new F<T, Boolean>() {
                        @Override
                        public Boolean apply(final T item) {
                            result.value = ((int)yieldItem(item));
                            return result.value == ((byte)0);
                        }
                    });
                }
            } else {
                if(beginYieldWithSize(((int)0)) == ((byte)1)) {
                    return ((int)((byte)1));
                } else {
                    items.goOn(new F<T, Boolean>() {
                        @Override
                        public Boolean apply(final T item) {
                            result.value = ((int)yieldItem(item));
                            return result.value == ((byte)0);
                        }
                    });
                }
            }
        }
        return endYieldWithResult(((int)result.value));
    }
    public Yield(final F<Integer, Integer> begin, final F<T, Integer> yield, final F<Integer, Integer> end, final F<Traversable<T>, Integer> all) {
        this.begin = begin;
        this.yield = yield;
        this.end = end;
        this.all = all;
    }
    static public <T> Yield<T> applyBeginYieldEnd(final F<Integer, Integer> begin, final F<T, Integer> yield, final F<Integer, Integer> end) {
        return applyBeginYieldEndAll(begin, yield, end, null);
    }
    static public <T> Yield<T> applyBeginYieldAll(final F<Integer, Integer> begin, final F<T, Integer> yield, final F<Traversable<T>, Integer> all) {
        return applyBeginYieldEndAll(begin, yield, null, all);
    }
    static public <T> Yield<T> applyBeginYield(final F<Integer, Integer> begin, final F<T, Integer> yield) {
        return applyBeginYieldEndAll(begin, yield, null, null);
    }
    static public <T> Yield<T> applyBeginEndAll(final F<Integer, Integer> begin, final F<Integer, Integer> end, final F<Traversable<T>, Integer> all) {
        return applyBeginYieldEndAll(begin, null, end, all);
    }
    static public <T> Yield<T> applyBeginEnd(final F<Integer, Integer> begin, final F<Integer, Integer> end) {
        return applyBeginYieldEndAll(begin, null, end, null);
    }
    static public <T> Yield<T> applyBeginAll(final F<Integer, Integer> begin, final F<Traversable<T>, Integer> all) {
        return applyBeginYieldEndAll(begin, null, null, all);
    }
    static public <T> Yield<T> applyBegin(final F<Integer, Integer> begin) {
        return applyBeginYieldEndAll(begin, null, null, null);
    }
    static public <T> Yield<T> applyYieldEndAll(final F<T, Integer> yield, final F<Integer, Integer> end, final F<Traversable<T>, Integer> all) {
        return applyBeginYieldEndAll(null, yield, end, all);
    }
    static public <T> Yield<T> applyYieldEnd(final F<T, Integer> yield, final F<Integer, Integer> end) {
        return applyBeginYieldEndAll(null, yield, end, null);
    }
    static public <T> Yield<T> applyYieldAll(final F<T, Integer> yield, final F<Traversable<T>, Integer> all) {
        return applyBeginYieldEndAll(null, yield, null, all);
    }
    static public <T> Yield<T> applyYield(final F<T, Integer> yield) {
        return applyBeginYieldEndAll(null, yield, null, null);
    }
    static public <T> Yield<T> applyEndAll(final F<Integer, Integer> end, final F<Traversable<T>, Integer> all) {
        return applyBeginYieldEndAll(null, null, end, all);
    }
    static public <T> Yield<T> applyEnd(final F<Integer, Integer> end) {
        return applyBeginYieldEndAll(null, null, end, null);
    }
    static public <T> Yield<T> applyAll(final F<Traversable<T>, Integer> all) {
        return applyBeginYieldEndAll(null, null, null, all);
    }
    static public <T> Yield<T> apply() {
        return applyBeginYieldEndAll(null, null, null, null);
    }
}