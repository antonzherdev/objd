package core.chain;

public class ZipLink<T, A, R> implements ChainLink<T, R> {
    public final Iterable<A> a;
    public final F2<T, A, R> f;
    @Override
    public Yield<A> buildYield(Yield<R> yield) {
        ERROR: Unknown local ai : Iterator#T<§A#G§> = <ZipLink#C<T#G, A#G, R#G>>self.<eIU>a\Iterable#T<§A#G§>\.<dIa>iterator\Iterator#T<§A#G§>\;
        return Yield().decorateBaseYield<A>(yield, new F<A, Integer>() {
            @Override
            public Integer f(A item) {
                ERROR: Unknown if(!(<l>ai\Iterator#T<§A#G§>\.<dIa>hasNext\bool\)) return 1
else return <l>yield\Yield#C<§R#G§>\.<dI>yield(item = <ZipLink#C<T#G, A#G, R#G>>self.<eIU>f\(§T#G§, §A#G§) -> §R#G§\.<d>apply( = <l>item\§A#G§\,  = <l>ai\Iterator#T<§A#G§>\.<dIa>next\§A#G§\)\§R#G§\)\int\;
            }
        });
    }
    public ZipLink(Iterable<A> a,F2<T, A, R> f) {
    }
}