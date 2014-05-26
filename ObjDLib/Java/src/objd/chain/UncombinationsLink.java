package objd.chain;

import objd.lang.*;
import objd.collection.MHashSet;
import objd.collection.Go;

public class UncombinationsLink<T> extends ChainLink_impl<Tuple<T, T>, T> {
    @Override
    public Yield<Tuple<T, T>> buildYield(final Yield<T> yield) {
        final MHashSet<T> set = new MHashSet<T>();
        return Yield.<Tuple<T, T>, T>decorateBaseYield(yield, new F<Tuple<T, T>, Go>() {
            @Override
            public Go apply(final Tuple<T, T> item) {
                Go r = Go.Continue;
                final T a = item.a;
                if(!(set.containsItem(a))) {
                    set.appendItem(a);
                    r = yield.yieldItem(a);
                }
                final T b = item.b;
                if(r == Go.Continue && !(set.containsItem(b))) {
                    set.appendItem(b);
                    r = yield.yieldItem(b);
                }
                return r;
            }
        });
    }
    public UncombinationsLink() {
    }
    public String toString() {
        return "UncombinationsLink";
    }
}