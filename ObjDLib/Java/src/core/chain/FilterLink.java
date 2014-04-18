package core.chain;

public class FilterLink<T> implements ChainLink<T, T> {
    public final F<T, Boolean> predicate;
    public final float selectivity;
    public Yield<T> buildYield(Yield<T> yield) {
        return ERROR: Unknown <to>Yield\Yield#C.class\.<dIt>decorate(base = <l>yield\Yield#C<§T#G§>\, begin = size : uint -> int = return <l>yield\Yield#C<§T#G§>\.<dI>beginYieldWith(size = (<l>size\uint\ * <FilterLink#C<T#G>>self.<eIU>selectivity\float4\).cast<uint>)\int\, yield = item : §^Traversable#T<T#G>§ -> int = {
    if(<FilterLink#C<T#G>>self.<eIU>predicate\§T#G§ -> bool\.<d>apply( = <l>item\§^Traversable#T<T#G>§\)\bool\) return <l>yield\Yield#C<§T#G§>\.<dI>yield(item = <l>item\§^Traversable#T<T#G>§\)\int\
else return 0
})\Yield#C<§^Traversable#T<T#G>§>\;
    }
    public FilterLink(F<T, Boolean> predicate,float selectivity) {
    }
    static final ClassType<FilterLink<T>> type;
}