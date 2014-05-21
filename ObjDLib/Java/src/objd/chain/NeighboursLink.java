package objd.chain;

import objd.lang.*;
import objd.collection.Go;

public class NeighboursLink<T> extends ChainLink_impl<T, Tuple<T, T>> {
    public final boolean ring;
    @Override
    public Yield<T> buildYield(final Yield<Tuple<T, T>> yield) {
        final Mut<T> first = new Mut<T>();
        final Mut<T> prev = new Mut<T>();
        return Yield.<T, Tuple<T, T>>decorateBaseBeginYieldEnd(yield, new F<Integer, Go>() {
            @Override
            public Go apply(final Integer size) {
                return yield.beginYieldWithSize(((size <= 1) ? (((int)(0))) : (((NeighboursLink.this.ring) ? (size) : (size - 1)))));
            }
        }, new F<T, Go>() {
            @Override
            public Go apply(final T item) {
                if(prev.value == null) {
                    first.value = item;
                    prev.value = item;
                    return Go.Continue;
                } else {
                    final T p = prev.value;
                    prev.value = item;
                    return yield.yieldItem(new Tuple<T, T>(p, item));
                }
            }
        }, ((this.ring) ? (new F<Go, Go>() {
            @Override
            public Go apply(final Go result) {
                if(result.equals(Go.Break) || first.value == null) {
                    return yield.endYieldWithResult(result);
                } else {
                    if(prev.value == null) {
                        throw new NullPointerException();
                    }
                    if(first.value == null) {
                        throw new NullPointerException();
                    }
                    return yield.endYieldWithResult(yield.yieldItem(new Tuple<T, T>(prev.value, first.value)));
                }
            }
        }) : (null)));
    }
    public NeighboursLink(final boolean ring) {
        this.ring = ring;
    }
}