package objd.chain;

import objd.lang.*;
import objd.collection.Iterable;
import objd.collection.Iterator;
import objd.collection.Go;

public class ZipLink<T, A, R> extends ChainLink_impl<T, R> {
    public final Iterable<A> a;
    public final F2<T, A, R> f;
    @Override
    public Yield<T> buildYield(final Yield<R> yield) {
        final Iterator<A> ai = this.a.iterator();
        return Yield.<T, R>decorateBaseYield(yield, new F<T, Go>() {
            @Override
            public Go apply(final T item) {
                if(!(ai.hasNext())) {
                    return Go.Break;
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
    public String toString() {
        return String.format("ZipLink(%s)", this.a);
    }
}