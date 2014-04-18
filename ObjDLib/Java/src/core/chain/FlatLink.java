package core.chain;

public class FlatLink<T> implements ChainLink<Traversable<T>, T> {
    public final float factor;
    public Yield<Traversable<T>> buildYield(Yield<T> yield) {
        return Yield().decorateBaseBeginYield<Traversable<T>>(yield, ERROR: Unknown size : uint -> int = return <l>yield\Yield#C<§T#G§>\.<dI>beginYieldWith(size = (<l>size\uint\ * <FlatLink#C<T#G>>self.<eIU>factor\float\).cast<uint>)\int\, ERROR: Unknown col : §^Traversable#T<T#G>§ -> int = {
    local var result : int = 0
    <l>col\§^Traversable#T<T#G>§\.<dIa>go(on = item : §T#G§ -> bool = if((<l>yield\Yield#C<§T#G§>\.<dI>yield(item = <l>item\§T#G§\)\int\ != 0)) {
    (<lm>result\int\ = 1)
    return False
}
else return True)\bool\
    return <lm>result\int\
});
    }
    public FlatLink(float factor) {
    }
    static final ClassType<FlatLink<T>> type;
}