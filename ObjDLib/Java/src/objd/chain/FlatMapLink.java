package objd.chain;

import objd.lang.*;
import objd.collection.Traversable;

public class FlatMapLink<A, B> extends ChainLink_impl<A, B> {
    public final F<A, Traversable<B>> f;
    public final float factor;
    @Override
    public Yield<A> buildYield(final Yield<B> yield) {
        return Yield.<A, B>decorateBaseBeginYield(yield, new F<Integer, Go>() {
            @Override
            public Go apply(final Integer size) {
                return yield.beginYieldWithSize(((int)(size * FlatMapLink.this.factor)));
            }
        }, new F<A, Go>() {
            @Override
            public Go apply(final A item) {
                return FlatMapLink.this.f.apply(item).goOn(new F<B, Go>() {
                    @Override
                    public Go apply(final B i) {
                        return yield.yieldItem(i);
                    }
                });
            }
        });
    }
    public FlatMapLink(final F<A, Traversable<B>> f, final float factor) {
        this.f = f;
        this.factor = factor;
    }
}