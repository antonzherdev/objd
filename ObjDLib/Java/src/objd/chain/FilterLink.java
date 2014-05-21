package objd.chain;

import objd.lang.*;
import objd.collection.Go;

public class FilterLink<T> extends ChainLink_impl<T, T> {
    public final F<T, Boolean> predicate;
    public final float factor;
    @Override
    public Yield<T> buildYield(final Yield<T> yield) {
        return Yield.<T, T>decorateBaseBeginYield(yield, new F<Integer, Go>() {
            @Override
            public Go apply(final Integer size) {
                return yield.beginYieldWithSize(((int)(size * FilterLink.this.factor)));
            }
        }, new F<T, Go>() {
            @Override
            public Go apply(final T item) {
                if(FilterLink.this.predicate.apply(item)) {
                    return yield.yieldItem(item);
                } else {
                    return Go.Continue;
                }
            }
        });
    }
    public FilterLink(final F<T, Boolean> predicate, final float factor) {
        this.predicate = predicate;
        this.factor = factor;
    }
}