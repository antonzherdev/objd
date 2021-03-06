package objd.chain;

import objd.lang.*;
import objd.collection.Traversable;
import objd.collection.Iterable;
import objd.collection.Go;

public class AppendLink<T> extends ChainLink_impl<T, T> {
    public final Traversable<T> collection;
    @Override
    public Yield<T> buildYield(final Yield<T> yield) {
        final Iterable<T> c = Util.<Iterable<T>>as(Iterable.class, this.collection);
        return Yield.<T, T>decorateBaseBeginEnd(yield, ((c != null) ? (new F<Integer, Go>() {
            @Override
            public Go apply(final Integer size) {
                return yield.beginYieldWithSize(size + c.count());
            }
        }) : (null)), new F<Go, Go>() {
            @Override
            public Go apply(final Go result) {
                if(result == Go.Continue) {
                    return yield.endYieldWithResult(AppendLink.this.collection.goOn(new F<T, Go>() {
                        @Override
                        public Go apply(final T item) {
                            return yield.yieldItem(item);
                        }
                    }));
                } else {
                    return yield.endYieldWithResult(result);
                }
            }
        });
    }
    public AppendLink(final Traversable<T> collection) {
        this.collection = collection;
    }
    public String toString() {
        return String.format("AppendLink(%s)", this.collection);
    }
}