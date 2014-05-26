package objd.chain;

import objd.lang.*;
import objd.collection.Go;

public class TopLink<T> extends ChainLink_impl<T, T> {
    public final int number;
    @Override
    public Yield<T> buildYield(final Yield<T> yield) {
        final Mut<Integer> n = new Mut<Integer>(((int)(0)));
        return Yield.<T, T>decorateBaseBeginYield(yield, new F<Integer, Go>() {
            @Override
            public Go apply(final Integer size) {
                return yield.beginYieldWithSize(UInt.maxB(size, TopLink.this.number));
            }
        }, new F<T, Go>() {
            @Override
            public Go apply(final T item) {
                if(n.value < TopLink.this.number) {
                    if(yield.yieldItem(item) == Go.Break) {
                        return Go.Break;
                    } else {
                        n.value++;
                        if(n.value < TopLink.this.number) {
                            return Go.Continue;
                        } else {
                            return Go.Break;
                        }
                    }
                } else {
                    return Go.Break;
                }
            }
        });
    }
    public TopLink(final int number) {
        this.number = number;
    }
}