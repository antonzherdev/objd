package core.chain;

public class FilterLink<T> implements ChainLink<T, T> {
    public final F<T, Boolean> predicate;
    public final float selectivity;
    @Override
    public Yield<T> buildYield(Yield<T> yield) {
        return Yield().decorateBaseBeginYield<Traversable<T>>(yield, new F<Integer, Integer>() {
            @Override
            public Integer apply(Integer size) {
                return yield.beginYieldWithSize(ERROR: Unknown (<l>size\uint\ * <FilterLink#C<T#G>>self.<eIU>selectivity\float4\).cast<uint>);
            }
        }, new F<Traversable<T>, Integer>() {
            @Override
            public Integer apply(Traversable<T> item) {
                if(FilterLink.this.predicate.apply(item)) {
                    return yield.yieldItem(item);
                } else {
                    return 0;
                }
            }
        });
    }
    public FilterLink(F<T, Boolean> predicate,float selectivity) {
        this.predicate = predicate;
        this.selectivity = selectivity;
    }
}