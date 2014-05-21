package objd.chain;

import objd.lang.*;
import objd.collection.Traversable;

public class FlatLink<T> extends ChainLink_impl<Traversable<T>, T> {
    public final float factor;
    @Override
    public Yield<Traversable<T>> buildYield(final Yield<T> yield) {
        return Yield.<Traversable<T>, T>decorateBaseBeginYield(yield, new F<Integer, Go>() {
            @Override
            public Go apply(final Integer size) {
                return yield.beginYieldWithSize(((int)(size * FlatLink.this.factor)));
            }
        }, new F<Traversable<T>, Go>() {
            @Override
            public Go apply(final Traversable<T> col) {
                return col.goOn(new F<T, Go>() {
                    @Override
                    public Go apply(final T item) {
                        return yield.yieldItem(item);
                    }
                });
            }
        });
    }
    public FlatLink(final float factor) {
        this.factor = factor;
    }
}