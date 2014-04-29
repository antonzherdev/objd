package objd.chain;

import objd.lang.*;
import objd.collection.MArray;

public class ShuffleLink<T> extends ChainLink_impl<T, T> {
    private MArray<T> _array;
    @Override
    public Yield<T> buildYield(final Yield<T> yield) {
        return Yield.<T, T>decorateBaseBeginYieldEnd(yield, new F<Integer, Integer>() {
            @Override
            public Integer apply(final Integer size) {
                ShuffleLink.this._array = new MArray<T>(size);
                return ((int)(0));
            }
        }, new F<T, Integer>() {
            @Override
            public Integer apply(final T item) {
                if(ShuffleLink.this._array == null) {
                    throw new NullPointerException();
                }
                if(ShuffleLink.this._array == null) {
                    throw new NullPointerException();
                }
                ShuffleLink.this._array.insertIndexItem(UInt.rndMax(ShuffleLink.this._array.count()), item);
                return ((int)(0));
            }
        }, new F<Integer, Integer>() {
            @Override
            public Integer apply(final Integer r) {
                if(ShuffleLink.this._array == null) {
                    throw new NullPointerException();
                }
                if(yield.yieldAllItems(ShuffleLink.this._array) == 1) {
                    return ((int)(1));
                } else {
                    return r;
                }
            }
        });
    }
    public ShuffleLink() {
    }
}