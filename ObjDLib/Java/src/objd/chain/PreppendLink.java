package objd.chain;

import objd.lang.*;
import objd.collection.Traversable;
import objd.collection.Iterable;
import objd.collection.Go;

public class PreppendLink<T> extends ChainLink_impl<T, T> {
    public final Traversable<T> collection;
    @Override
    public Yield<T> buildYield(final Yield<T> yield) {
        return Yield.<T, T>decorateBaseBegin(yield, new F<Integer, Go>() {
            @Override
            public Go apply(final Integer size) {
                final Integer __tmprp1_0rp0b;
                {
                    final Iterable<T> _ = PreppendLink.this.collection.ERROR: Unknown as<Iterable#T<T#G>>;
                    if(_ != null) {
                        __tmprp1_0rp0b = _.count();
                    } else {
                        __tmprp1_0rp0b = null;
                    }
                }
                final Go r = yield.beginYieldWithSize(size + ((__tmprp1_0rp0b != null) ? (__tmprp1_0rp0b) : (((int)(0)))));
                if(r.equals(Go.Break)) {
                    return Go.Break;
                } else {
                    return PreppendLink.this.collection.goOn(new F<T, Go>() {
                        @Override
                        public Go apply(final T item) {
                            return yield.yieldItem(item);
                        }
                    });
                }
            }
        });
    }
    public PreppendLink(final Traversable<T> collection) {
        this.collection = collection;
    }
}