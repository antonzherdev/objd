package core.chain;

public class FlatLink<T> implements ChainLink<Traversable<T>, T> {
    public final float factor;
    @Override
    public Yield<Traversable<T>> buildYield(Yield<T> yield) {
        return Yield().decorateBaseBeginYield<Traversable<T>>(yield, new F<Integer, Integer>() {
            @Override
            public Integer f(Integer size) {
                return yield.beginYieldWithSize(ERROR: Unknown (<l>size\uint\ * <FlatLink#C<T#G>>self.<eIU>factor\float\).cast<uint>);
            }
        }, new F<Traversable<T>, Integer>() {
            @Override
            public Integer f(Traversable<T> col) {
                ERROR: Unknown local var result : int = 0;
                col.goOn(new F<T, Boolean>() {
                    @Override
                    public Boolean f(T item) {
                        ERROR: Unknown if((<l>yield\Yield#C<§T#G§>\.<dI>yield(item = <l>item\§T#G§\)\int\ != 0)) {
    (<lm>result\int\ = 1)
    return False
}
else return True;
                    }
                });
                return result;
            }
        });
    }
    public FlatLink(float factor) {
    }
}