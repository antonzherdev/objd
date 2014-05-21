package objd.chain;

import objd.lang.*;
import objd.collection.MList;
import objd.collection.Go;

public class ReverseLink<T> extends ChainLink_impl<T, T> {
    @Override
    public Yield<T> buildYield(final Yield<T> yield) {
        final MList<T> list = new MList<T>();
        return Yield.<T, T>decorateBaseBeginYieldEnd(yield, new F<Integer, Go>() {
            @Override
            public Go apply(final Integer _) {
                return Go.Continue;
            }
        }, new F<T, Go>() {
            @Override
            public Go apply(final T item) {
                list.prependItem(item);
                return Go.Continue;
            }
        }, new F<Go, Go>() {
            @Override
            public Go apply(final Go result) {
                if(result.equals(Go.Break)) {
                    return yield.endYieldWithResult(result);
                } else {
                    return yield.yieldAllItems(list);
                }
            }
        });
    }
    public ReverseLink() {
    }
}