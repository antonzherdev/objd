package objd.chain;

import objd.lang.*;
import objd.collection.MArray;
import objd.collection.Go;

public class CombinationsLink<T> extends ChainLink_impl<T, Tuple<T, T>> {
    private int sfN(final int n) {
        int i = 1;
        int r = 0;
        while(i < n) {
            r += i;
            i++;
        }
        return ((int)(r));
    }
    @Override
    public Yield<T> buildYield(final Yield<Tuple<T, T>> yield) {
        final Mut<MArray<T>> prevs = new Mut<MArray<T>>();
        return Yield.<T, Tuple<T, T>>decorateBaseBeginYield(yield, new F<Integer, Go>() {
            @Override
            public Go apply(final Integer size) {
                prevs.value = new MArray<T>(size);
                return yield.beginYieldWithSize(sfN(size));
            }
        }, new F<T, Go>() {
            @Override
            public Go apply(final T item) {
                if(prevs.value == null) {
                    throw new NullPointerException();
                }
                final Go r = prevs.value.goOn(new F<T, Go>() {
                    @Override
                    public Go apply(final T prev) {
                        return yield.yieldItem(new Tuple<T, T>(prev, item));
                    }
                });
                if(prevs.value == null) {
                    throw new NullPointerException();
                }
                prevs.value.appendItem(item);
                return r;
            }
        });
    }
    public CombinationsLink() {
    }
}