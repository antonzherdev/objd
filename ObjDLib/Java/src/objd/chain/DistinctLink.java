package objd.chain;

import objd.lang.*;
import objd.collection.MHashSet;
import objd.collection.Go;

public class DistinctLink<T> extends ChainLink_impl<T, T> {
    public final double factor;
    @Override
    public Yield<T> buildYield(final Yield<T> yield) {
        final MHashSet<T> set = new MHashSet<T>();
        return Yield.<T, T>decorateBaseBeginYield(yield, new F<Integer, Go>() {
            @Override
            public Go apply(final Integer size) {
                return yield.beginYieldWithSize(((int)(size * DistinctLink.this.factor)));
            }
        }, new F<T, Go>() {
            @Override
            public Go apply(final T item) {
                if(set.containsItem(item)) {
                    return Go.Continue;
                } else {
                    set.appendItem(item);
                    return yield.yieldItem(item);
                }
            }
        });
    }
    public DistinctLink(final double factor) {
        this.factor = factor;
    }
    public String toString() {
        return String.format("DistinctLink(%f)", this.factor);
    }
}