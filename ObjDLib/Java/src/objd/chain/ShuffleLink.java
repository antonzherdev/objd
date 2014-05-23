package objd.chain;

import objd.lang.*;
import objd.collection.MArray;
import objd.collection.Go;

public class ShuffleLink<T> extends ChainLink_impl<T, T> {
    private MArray<T> _array;
    @Override
    public Yield<T> buildYield(final Yield<T> yield) {
        return Yield.<T, T>decorateBaseBeginYieldEnd(yield, new F<Integer, Go>() {
            @Override
            public Go apply(final Integer size) {
                ShuffleLink.this._array = new MArray<T>(size);
                return Go.Continue;
            }
        }, new F<T, Go>() {
            @Override
            public Go apply(final T item) {
                if(ShuffleLink.this._array == null) {
                    throw new NullPointerException();
                }
                if(ShuffleLink.this._array == null) {
                    throw new NullPointerException();
                }
                ShuffleLink.this._array.insertIndexItem(UInt.rndMax(ShuffleLink.this._array.count()), item);
                return Go.Continue;
            }
        }, new F<Go, Go>() {
            @Override
            public Go apply(final Go r) {
                if(r == Go.Break) {
                    return yield.endYieldWithResult(r);
                } else {
                    if(ShuffleLink.this._array == null) {
                        throw new NullPointerException();
                    }
                    return yield.yieldAllItems(ShuffleLink.this._array);
                }
            }
        });
    }
    public ShuffleLink() {
    }
}