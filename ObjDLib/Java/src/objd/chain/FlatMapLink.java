package objd.chain;

import objd.lang.*;
import objd.collection.Traversable;
import objd.collection.Go;

public class FlatMapLink<A, B> extends ChainLink_impl<A, B> {
    public final double factor;
    public final F<A, Traversable<B>> f;
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
    public FlatMapLink(final double factor, final F<A, Traversable<B>> f) {
        this.factor = factor;
        this.f = f;
    }
}