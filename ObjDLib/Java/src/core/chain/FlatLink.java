package core.chain;

public class FlatLink<T> implements ChainLink<Traversable<T>, T> {
    public final float factor;
    @Override
    public Yield<Traversable<T>> buildYield(Yield<T> yield) {
        return Yield().decorateBaseBeginYield<Traversable<T>>(yield, new F<Integer, Integer>() {
            @Override
            public Integer apply(Integer size) {
                return yield.beginYieldWithSize(ERROR: Unknown (<l>size\uint\ * <FlatLink#C<T#G>>self.<eIU>factor\float\).cast<uint>);
            }
        }, new F<Traversable<T>, Integer>() {
            @Override
            public Integer apply(Traversable<T> col) {
                int result = 0;
                col.goOn(new F<T, Boolean>() {
                    @Override
                    public Boolean apply(T item) {
                        if(yield.yieldItem(item).equals(0)) {
                            result = 1;
                            return false;
                        } else {
                            return true;
                        }
                    }
                });
                return result;
            }
        });
    }
    public FlatLink(float factor) {
        this.factor = factor;
    }
}