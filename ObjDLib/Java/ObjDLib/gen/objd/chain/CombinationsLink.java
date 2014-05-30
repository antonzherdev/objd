package objd.chain;

import objd.lang.*;
import objd.collection.MArray;
import objd.collection.Go;
import objd.collection.Iterator;

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
                final Go r;
                {
                    Go __il__1r_0ret = Go.Continue;
                    if(prevs.value == null) {
                        throw new NullPointerException();
                    }
                    final Iterator<T> __il__1r_0i = prevs.value.iterator();
                    while(__il__1r_0i.hasNext()) {
                        final T prev = __il__1r_0i.next();
                        if(yield.yieldItem(new Tuple<T, T>(prev, item)) == Go.Break) {
                            __il__1r_0ret = Go.Break;
                            break;
                        }
                    }
                    r = __il__1r_0ret;
                }
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
    public String toString() {
        return "CombinationsLink";
    }
}