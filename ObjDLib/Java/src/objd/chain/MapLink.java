package objd.chain;

import objd.lang.*;
import objd.collection.Go;

public class MapLink<A, B> extends ChainLink_impl<A, B> {
    public final F<A, B> f;
    @Override
    public Yield<A> buildYield(final Yield<B> yield) {
        return Yield.<A, B>decorateBaseYield(yield, new F<A, Go>() {
            @Override
            public Go apply(final A item) {
                return yield.yieldItem(MapLink.this.f.apply(item));
            }
        });
    }
    public MapLink(final F<A, B> f) {
        this.f = f;
    }
}