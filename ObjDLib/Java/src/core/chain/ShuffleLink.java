package core.chain;

public class ShuffleLink<T> implements ChainLink<T, T> {
    private MArray<T> _array;
    @Override
    public Yield<T> buildYield(Yield<T> yield) {
        return Yield().decorateBaseBeginYieldEnd<T>(yield, new F<Integer, Integer>() {
            @Override
            public Integer apply(Integer size) {
                ShuffleLink.this._array = new MArray<T>(size);
                return 0;
            }
        }, new F<T, Integer>() {
            @Override
            public Integer apply(T item) {
                if(ShuffleLink.this._array == null) {
                    throw new RuntimeException("Not null");
                } else {
                    ShuffleLink.this._array;
                }
                if(ShuffleLink.this._array == null) {
                    throw new RuntimeException("Not null");
                } else {
                    ShuffleLink.this._array;
                }
                .insertIndexItem(UInt().rndMax(.count()), item);
                return 0;
            }
        }, new F<Integer, Integer>() {
            @Override
            public Integer apply(Integer r) {
                if(ShuffleLink.this._array == null) {
                    throw new RuntimeException("Not null");
                } else {
                    ShuffleLink.this._array;
                }
                if(yield.yieldAll().equals(1)) {
                    return 1;
                } else {
                    return r;
                }
            }
        });
    }
    public ShuffleLink() {
    }
}