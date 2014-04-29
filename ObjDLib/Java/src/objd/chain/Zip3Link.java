package objd.chain;

import objd.lang.*;
import objd.collection.Iterable;
import objd.collection.Iterator;

public class Zip3Link<T, A, B, R> extends ChainLink_impl<T, R> {
    public final Iterable<A> a;
    public final Iterable<B> b;
    public final F3<T, A, B, R> f;
    @Override
    public Yield<A> buildYield(final Yield<R> yield) {
        final Iterator<A> ai = this.a.iterator();
        final Iterator<B> bi = this.b.iterator();
        return Yield.<A>decorateBaseYield(yield, new F<A, Integer>() {
            @Override
            public Integer apply(final A item) {
                if(!(ai.hasNext()) || !(bi.hasNext())) {
                    return ((int)1);
                } else {
                    return yield.yieldItem(Zip3Link.this.f.apply(item, ai.next(), bi.next()));
                }
            }
        });
    }
    public Zip3Link(final Iterable<A> a, final Iterable<B> b, final F3<T, A, B, R> f) {
        this.a = a;
        this.b = b;
        this.f = f;
    }
}