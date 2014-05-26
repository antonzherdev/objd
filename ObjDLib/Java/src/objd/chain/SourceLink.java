package objd.chain;

import objd.lang.*;
import objd.collection.Traversable;
import objd.collection.Iterable;
import objd.collection.Go;

public class SourceLink<T> extends ChainLink_impl<Void, T> {
    public final Traversable<T> collection;
    @Override
    public Yield<Void> buildYield(final Yield<T> yield) {
        final Iterable<T> c = Util.<Iterable<T>>as(Iterable.class, this.collection);
        return Yield.<Void>makeBeginEnd(((c != null) ? (new F<Integer, Go>() {
            @Override
            public Go apply(final Integer size) {
                return yield.beginYieldWithSize(c.count());
            }
        }) : (null)), new F<Go, Go>() {
            @Override
            public Go apply(final Go result) {
                if(result == Go.Break) {
                    return yield.endYieldWithResult(result);
                } else {
                    return yield.yieldAllItems(SourceLink.this.collection);
                }
            }
        });
    }
    public SourceLink(final Traversable<T> collection) {
        this.collection = collection;
    }
    public String toString() {
        return String.format("SourceLink(%s)", this.collection);
    }
}