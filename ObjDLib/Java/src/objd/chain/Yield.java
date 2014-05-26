package objd.chain;

import objd.lang.*;
import objd.collection.Go;
import objd.collection.Traversable;
import objd.collection.Array;
import objd.collection.Iterable;
import objd.collection.Iterator;
import objd.collection.HashMap;

public class Yield<T> {
    public final F<Integer, Go> begin;
    public final F<T, Go> yield;
    public final F<Go, Go> end;
    public final F2<Yield<T>, Traversable<T>, Go> all;
    public static <T> Yield<T> makeBeginYieldEndAll(final F<Integer, Go> begin, final F<T, Go> yield, final F<Go, Go> end, final F2<Yield<T>, Traversable<T>, Go> all) {
        return new Yield<T>(begin, yield, end, all);
    }
    public static <T> Yield<T> makeBeginYieldEnd(final F<Integer, Go> begin, final F<T, Go> yield, final F<Go, Go> end) {
        return Yield.<T>makeBeginYieldEndAll(begin, yield, end, null);
    }
    public static <T> Yield<T> makeBeginYieldAll(final F<Integer, Go> begin, final F<T, Go> yield, final F2<Yield<T>, Traversable<T>, Go> all) {
        return Yield.<T>makeBeginYieldEndAll(begin, yield, null, all);
    }
    public static <T> Yield<T> makeBeginYield(final F<Integer, Go> begin, final F<T, Go> yield) {
        return Yield.<T>makeBeginYieldEndAll(begin, yield, null, null);
    }
    public static <T> Yield<T> makeBeginEndAll(final F<Integer, Go> begin, final F<Go, Go> end, final F2<Yield<T>, Traversable<T>, Go> all) {
        return Yield.<T>makeBeginYieldEndAll(begin, null, end, all);
    }
    public static <T> Yield<T> makeBeginEnd(final F<Integer, Go> begin, final F<Go, Go> end) {
        return Yield.<T>makeBeginYieldEndAll(begin, null, end, null);
    }
    public static <T> Yield<T> makeBeginAll(final F<Integer, Go> begin, final F2<Yield<T>, Traversable<T>, Go> all) {
        return Yield.<T>makeBeginYieldEndAll(begin, null, null, all);
    }
    public static <T> Yield<T> makeBegin(final F<Integer, Go> begin) {
        return Yield.<T>makeBeginYieldEndAll(begin, null, null, null);
    }
    public static <T> Yield<T> makeYieldEndAll(final F<T, Go> yield, final F<Go, Go> end, final F2<Yield<T>, Traversable<T>, Go> all) {
        return Yield.<T>makeBeginYieldEndAll(null, yield, end, all);
    }
    public static <T> Yield<T> makeYieldEnd(final F<T, Go> yield, final F<Go, Go> end) {
        return Yield.<T>makeBeginYieldEndAll(null, yield, end, null);
    }
    public static <T> Yield<T> makeYieldAll(final F<T, Go> yield, final F2<Yield<T>, Traversable<T>, Go> all) {
        return Yield.<T>makeBeginYieldEndAll(null, yield, null, all);
    }
    public static <T> Yield<T> makeYield(final F<T, Go> yield) {
        return Yield.<T>makeBeginYieldEndAll(null, yield, null, null);
    }
    public static <T> Yield<T> makeEndAll(final F<Go, Go> end, final F2<Yield<T>, Traversable<T>, Go> all) {
        return Yield.<T>makeBeginYieldEndAll(null, null, end, all);
    }
    public static <T> Yield<T> makeEnd(final F<Go, Go> end) {
        return Yield.<T>makeBeginYieldEndAll(null, null, end, null);
    }
    public static <T> Yield<T> makeAll(final F2<Yield<T>, Traversable<T>, Go> all) {
        return Yield.<T>makeBeginYieldEndAll(null, null, null, all);
    }
    public static <T> Yield<T> make() {
        return Yield.<T>makeBeginYieldEndAll(null, null, null, null);
    }
    public static <T, A> Yield<T> decorateBaseBeginYieldEndAll(final Yield<A> base, final F<Integer, Go> begin, final F<T, Go> yield, final F<Go, Go> end, final F2<Yield<T>, Traversable<T>, Go> all) {
        return new Yield<T>(((begin != null) ? (begin) : (new F<Integer, Go>() {
            @Override
            public Go apply(final Integer size) {
                return base.beginYieldWithSize(size);
            }
        })), ((yield != null) ? (yield) : (new F<T, Go>() {
            @Override
            public Go apply(final T item) {
                return base.yieldItem(((A)(item)));
            }
        })), ((end != null) ? (end) : (new F<Go, Go>() {
            @Override
            public Go apply(final Go result) {
                return base.endYieldWithResult(result);
            }
        })), all);
    }
    public static <T, A> Yield<T> decorateBaseBeginYieldEnd(final Yield<A> base, final F<Integer, Go> begin, final F<T, Go> yield, final F<Go, Go> end) {
        return Yield.<T, A>decorateBaseBeginYieldEndAll(base, begin, yield, end, null);
    }
    public static <T, A> Yield<T> decorateBaseBeginYieldAll(final Yield<A> base, final F<Integer, Go> begin, final F<T, Go> yield, final F2<Yield<T>, Traversable<T>, Go> all) {
        return Yield.<T, A>decorateBaseBeginYieldEndAll(base, begin, yield, null, all);
    }
    public static <T, A> Yield<T> decorateBaseBeginYield(final Yield<A> base, final F<Integer, Go> begin, final F<T, Go> yield) {
        return Yield.<T, A>decorateBaseBeginYieldEndAll(base, begin, yield, null, null);
    }
    public static <T, A> Yield<T> decorateBaseBeginEndAll(final Yield<A> base, final F<Integer, Go> begin, final F<Go, Go> end, final F2<Yield<T>, Traversable<T>, Go> all) {
        return Yield.<T, A>decorateBaseBeginYieldEndAll(base, begin, null, end, all);
    }
    public static <T, A> Yield<T> decorateBaseBeginEnd(final Yield<A> base, final F<Integer, Go> begin, final F<Go, Go> end) {
        return Yield.<T, A>decorateBaseBeginYieldEndAll(base, begin, null, end, null);
    }
    public static <T, A> Yield<T> decorateBaseBeginAll(final Yield<A> base, final F<Integer, Go> begin, final F2<Yield<T>, Traversable<T>, Go> all) {
        return Yield.<T, A>decorateBaseBeginYieldEndAll(base, begin, null, null, all);
    }
    public static <T, A> Yield<T> decorateBaseBegin(final Yield<A> base, final F<Integer, Go> begin) {
        return Yield.<T, A>decorateBaseBeginYieldEndAll(base, begin, null, null, null);
    }
    public static <T, A> Yield<T> decorateBaseYieldEndAll(final Yield<A> base, final F<T, Go> yield, final F<Go, Go> end, final F2<Yield<T>, Traversable<T>, Go> all) {
        return Yield.<T, A>decorateBaseBeginYieldEndAll(base, null, yield, end, all);
    }
    public static <T, A> Yield<T> decorateBaseYieldEnd(final Yield<A> base, final F<T, Go> yield, final F<Go, Go> end) {
        return Yield.<T, A>decorateBaseBeginYieldEndAll(base, null, yield, end, null);
    }
    public static <T, A> Yield<T> decorateBaseYieldAll(final Yield<A> base, final F<T, Go> yield, final F2<Yield<T>, Traversable<T>, Go> all) {
        return Yield.<T, A>decorateBaseBeginYieldEndAll(base, null, yield, null, all);
    }
    public static <T, A> Yield<T> decorateBaseYield(final Yield<A> base, final F<T, Go> yield) {
        return Yield.<T, A>decorateBaseBeginYieldEndAll(base, null, yield, null, null);
    }
    public static <T, A> Yield<T> decorateBaseEndAll(final Yield<A> base, final F<Go, Go> end, final F2<Yield<T>, Traversable<T>, Go> all) {
        return Yield.<T, A>decorateBaseBeginYieldEndAll(base, null, null, end, all);
    }
    public static <T, A> Yield<T> decorateBaseEnd(final Yield<A> base, final F<Go, Go> end) {
        return Yield.<T, A>decorateBaseBeginYieldEndAll(base, null, null, end, null);
    }
    public static <T, A> Yield<T> decorateBaseAll(final Yield<A> base, final F2<Yield<T>, Traversable<T>, Go> all) {
        return Yield.<T, A>decorateBaseBeginYieldEndAll(base, null, null, null, all);
    }
    public static <T, A> Yield<T> decorateBase(final Yield<A> base) {
        return Yield.<T, A>decorateBaseBeginYieldEndAll(base, null, null, null, null);
    }
    public Go beginYieldWithSize(final int size) {
        if(this.begin == null) {
            return Go.Continue;
        } else {
            return this.begin.apply(size);
        }
    }
    public Go yieldItem(final T item) {
        if(this.yield == null) {
            return Go.Continue;
        } else {
            return this.yield.apply(item);
        }
    }
    public Go endYieldWithResult(final Go result) {
        if(this.end == null) {
            return result;
        } else {
            return this.end.apply(result);
        }
    }
    public Go yieldAllItems(final Traversable<T> items) {
        if(this.all != null) {
            return this.all.apply(this, items);
        } else {
            return stdYieldAllItems(items);
        }
    }
    public Go stdYieldAllItems(final Traversable<T> items) {
        final Mut<Go> result = new Mut<Go>(Go.Continue);
        if(items instanceof Array) {
            final Array<T> _items = ((Array<T>)(((Array)(items))));
            if(beginYieldWithSize(_items.count()) == Go.Continue) {
                _items.goOn(new F<T, Go>() {
                    @Override
                    public Go apply(final T item) {
                        result.value = yieldItem(item);
                        return result.value;
                    }
                });
            }
        } else {
            if(items instanceof HashMap) {
                final Iterable<T> _items = ((Iterable<T>)(((Iterable)(items))));
                if(beginYieldWithSize(_items.count()) == Go.Continue) {
                    items.goOn(new F<T, Go>() {
                        @Override
                        public Go apply(final T item) {
                            result.value = yieldItem(item);
                            return result.value;
                        }
                    });
                }
            } else {
                if(items instanceof Iterable) {
                    final Iterable<T> _items = ((Iterable<T>)(((Iterable)(items))));
                    if(beginYieldWithSize(_items.count()) == Go.Continue) {
                        Go __il__1fft_1tret = Go.Continue;
                        final Iterator<T> __il__1fft_1ti = _items.iterator();
                        while(__il__1fft_1ti.hasNext()) {
                            final T item = __il__1fft_1ti.next();
                            result.value = yieldItem(item);
                            if(result.value == Go.Break) {
                                __il__1fft_1tret = Go.Break;
                                break;
                            }
                        }
                        return __il__1fft_1tret;
                    }
                } else {
                    if(beginYieldWithSize(((int)(0))) == Go.Continue) {
                        items.goOn(new F<T, Go>() {
                            @Override
                            public Go apply(final T item) {
                                result.value = yieldItem(item);
                                return result.value;
                            }
                        });
                    }
                }
            }
        }
        return endYieldWithResult(result.value);
    }
    public Yield(final F<Integer, Go> begin, final F<T, Go> yield, final F<Go, Go> end, final F2<Yield<T>, Traversable<T>, Go> all) {
        this.begin = begin;
        this.yield = yield;
        this.end = end;
        this.all = all;
    }
    static public <T> Yield<T> applyBeginYieldEnd(final F<Integer, Go> begin, final F<T, Go> yield, final F<Go, Go> end) {
        return new Yield<T>(begin, yield, end, null);
    }
    static public <T> Yield<T> applyBeginYieldAll(final F<Integer, Go> begin, final F<T, Go> yield, final F2<Yield<T>, Traversable<T>, Go> all) {
        return new Yield<T>(begin, yield, null, all);
    }
    static public <T> Yield<T> applyBeginYield(final F<Integer, Go> begin, final F<T, Go> yield) {
        return new Yield<T>(begin, yield, null, null);
    }
    static public <T> Yield<T> applyBeginEndAll(final F<Integer, Go> begin, final F<Go, Go> end, final F2<Yield<T>, Traversable<T>, Go> all) {
        return new Yield<T>(begin, null, end, all);
    }
    static public <T> Yield<T> applyBeginEnd(final F<Integer, Go> begin, final F<Go, Go> end) {
        return new Yield<T>(begin, null, end, null);
    }
    static public <T> Yield<T> applyBeginAll(final F<Integer, Go> begin, final F2<Yield<T>, Traversable<T>, Go> all) {
        return new Yield<T>(begin, null, null, all);
    }
    static public <T> Yield<T> applyBegin(final F<Integer, Go> begin) {
        return new Yield<T>(begin, null, null, null);
    }
    static public <T> Yield<T> applyYieldEndAll(final F<T, Go> yield, final F<Go, Go> end, final F2<Yield<T>, Traversable<T>, Go> all) {
        return new Yield<T>(null, yield, end, all);
    }
    static public <T> Yield<T> applyYieldEnd(final F<T, Go> yield, final F<Go, Go> end) {
        return new Yield<T>(null, yield, end, null);
    }
    static public <T> Yield<T> applyYieldAll(final F<T, Go> yield, final F2<Yield<T>, Traversable<T>, Go> all) {
        return new Yield<T>(null, yield, null, all);
    }
    static public <T> Yield<T> applyYield(final F<T, Go> yield) {
        return new Yield<T>(null, yield, null, null);
    }
    static public <T> Yield<T> applyEndAll(final F<Go, Go> end, final F2<Yield<T>, Traversable<T>, Go> all) {
        return new Yield<T>(null, null, end, all);
    }
    static public <T> Yield<T> applyEnd(final F<Go, Go> end) {
        return new Yield<T>(null, null, end, null);
    }
    static public <T> Yield<T> applyAll(final F2<Yield<T>, Traversable<T>, Go> all) {
        return new Yield<T>(null, null, null, all);
    }
    static public <T> Yield<T> apply() {
        return new Yield<T>(null, null, null, null);
    }
}