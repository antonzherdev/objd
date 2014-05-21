package objd.chain;

import objd.lang.*;
import objd.collection.Go;

public class MapOptLink<A, B> extends ChainLink_impl<A, B> {
    public final F<A, B> f;
    @Override
    public Yield<A> buildYield(final Yield<B> yield) {
        return Yield.<A, B>decorateBaseYield(yield, new F<A, Go>() {
            @Override
            public Go apply(final A item) {
                final Go __tmp_0r;
                {
                    final B _ = MapOptLink.this.f.apply(item);
                    if(_ != null) {
                        __tmp_0r = yield.yieldItem(_);
                    } else {
                        __tmp_0r = null;
                    }
                }
                if(__tmp_0r != null) {
                    return __tmp_0r;
                } else {
                    return Go.Continue;
                }
            }
        });
    }
    public MapOptLink(final F<A, B> f) {
        this.f = f;
    }
}