package core.chain;

public class FlatLink<T> implements ChainLink<Traversable<T>, T> {
    public final float factor;
    public Yield<Traversable<T>> buildYield(Yield<T> yield) {
        return ERROR: Unknown <to>Yield\Yield#C.class\.<dIt>decorate(base = <l>yield\Yield#C<§T#G§>\, begin = size : uint -> int = return <l>yield\Yield#C<§T#G§>\.<dI>beginYieldWith(size = (<l>size\uint\ * <FlatLink#C<T#G>>self.<eIU>factor\float\).cast<uint>)\int\, yield = col : §^Traversable#T<T#G>§ -> int = {
    local var result : int = 0
    <l>col\§^Traversable#T<T#G>§\.<dIa>go(on = item : §T#G§ -> bool = if((<l>yield\Yield#C<§T#G§>\.<dI>yield(item = <l>item\§T#G§\)\int\ != 0)) {
    (<lm>result\int\ = 1)
    return False
}
else return True)\bool\
    return <lm>result\int\
})\Yield#C<§^Traversable#T<T#G>§>\;
    }
    public FlatLink(float factor) {
    }
    static final ClassType<FlatLink<T>> type;
}