package objd.chain;

import objd.lang.*;
import objd.collection.Iterable;
import objd.collection.Iterator;

public class ZipLink<T, A, R> extends ChainLink_impl<T, R> {
    public final Iterable<A> a;
    public final F2<T, A, R> f;
    @Override
    public Yield<A> buildYield(final Yield<R> yield) {
        final Iterator<A> ai = this.a.iterator();
        return Yield.<A>decorateBaseYield(yield, new F<A, Integer>() {
            @Override
            public Integer apply(final A item) {
                if(!(ai.hasNext())) {
                    return ((int)(1));
                } else {
                    return yield.yieldItem(ZipLink.this.f.apply(item, ai.next()));
                }
            }
        });
    }
    public ZipLink(final Iterable<A> a, final F2<T, A, R> f) {
        this.a = a;
        this.f = f;
    }
}