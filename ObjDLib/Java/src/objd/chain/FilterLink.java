package objd.chain;

import objd.lang.*;
import objd.collection.Traversable;

public class FilterLink<T> extends ChainLink_impl<T, T> {
    public final F<T, Boolean> predicate;
    public final float selectivity;
    @Override
    public Yield<T> buildYield(final Yield<T> yield) {
        return Yield.<Traversable<T>>decorateBaseBeginYield(yield, new F<Integer, Integer>() {
            @Override
            public Integer apply(final Integer size) {
                return yield.beginYieldWithSize(((int)size * FilterLink.this.selectivity));
            }
        }, new F<Traversable<T>, Integer>() {
            @Override
            public Integer apply(final Traversable<T> item) {
                if(FilterLink.this.predicate.apply(item)) {
                    return yield.yieldItem(item);
                } else {
                    return ((int)0);
                }
            }
        });
    }
    public FilterLink(final F<T, Boolean> predicate, final float selectivity) {
        this.predicate = predicate;
        this.selectivity = selectivity;
    }
}