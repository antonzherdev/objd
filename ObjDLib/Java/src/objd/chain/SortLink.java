package objd.chain;

import objd.lang.*;
import objd.collection.MTreeSet;
import objd.collection.Go;

public class SortLink<T> extends ChainLink_impl<T, T> {
    public final F2<T, T, Integer> comparator;
    @Override
    public Yield<T> buildYield(final Yield<T> yield) {
        final MTreeSet<T> set = MTreeSet.<T>applyComparator(this.comparator);
        return Yield.<T, T>decorateBaseBeginYieldEnd(yield, new F<Integer, Go>() {
            @Override
            public Go apply(final Integer _) {
                return Go.Continue;
            }
        }, new F<T, Go>() {
            @Override
            public Go apply(final T item) {
                set.appendItem(item);
                return Go.Continue;
            }
        }, new F<Go, Go>() {
            @Override
            public Go apply(final Go result) {
                if(result == Go.Break) {
                    return yield.endYieldWithResult(result);
                } else {
                    return yield.yieldAllItems(set);
                }
            }
        });
    }
    public SortLink(final F2<T, T, Integer> comparator) {
        this.comparator = comparator;
    }
}