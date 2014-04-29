package objd.chain;

import objd.lang.*;
import objd.collection.Traversable;

public class FlatLink<T> extends ChainLink_impl<Traversable<T>, T> {
    public final float factor;
    @Override
    public Yield<Traversable<T>> buildYield(final Yield<T> yield) {
        return Yield.<Traversable<T>, T>decorateBaseBeginYield(yield, new F<Integer, Integer>() {
            @Override
            public Integer apply(final Integer size) {
                return yield.beginYieldWithSize(((int)(size * FlatLink.this.factor)));
            }
        }, new F<Traversable<T>, Integer>() {
            @Override
            public Integer apply(final Traversable<T> col) {
                final Mut<Integer> result = new Mut<Integer>(0);
                col.goOn(new F<T, Boolean>() {
                    @Override
                    public Boolean apply(final T item) {
                        if(yield.yieldItem(item) != 0) {
                            result.value = 1;
                            return false;
                        } else {
                            return true;
                        }
                    }
                });
                return ((int)(result.value));
            }
        });
    }
    public FlatLink(final float factor) {
        this.factor = factor;
    }
}