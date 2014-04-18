package core.chain;

public class ZipLink<T, A, R> implements ChainLink<T, R> {
    public final Iterable<A> a;
    public final F2<T, A, R> f;
    public Yield<A> buildYield(Yield<R> yield) {
        ERROR: Unknown local ai : Iterator#T<§A#G§> = <ZipLink#C<T#G, A#G, R#G>>self.<eIU>a\Iterable#T<§A#G§>\.<dIa>iterator\Iterator#T<§A#G§>\;
        return ERROR: Unknown <to>Yield\Yield#C.class\.<dIt>decorate(base = <l>yield\Yield#C<§R#G§>\, yield = item : §A#G§ -> int = {
    if(!(<l>ai\Iterator#T<§A#G§>\.<dIa>hasNext\bool\)) return 1
else return <l>yield\Yield#C<§R#G§>\.<dI>yield(item = <ZipLink#C<T#G, A#G, R#G>>self.<eIU>f\(§T#G§, §A#G§) -> §R#G§\.<d>apply( = <l>item\§A#G§\,  = <l>ai\Iterator#T<§A#G§>\.<dIa>next\§A#G§\)\§R#G§\)\int\
})\Yield#C<§A#G§>\;
    }
    public ZipLink(Iterable<A> a,F2<T, A, R> f) {
    }
    static final ClassType<ZipLink<T, A, R>> type;
}