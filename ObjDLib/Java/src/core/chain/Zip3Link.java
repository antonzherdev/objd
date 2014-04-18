package core.chain;

public class Zip3Link<T, A, B, R> implements ChainLink<T, R> {
    public final Iterable<A> a;
    public final Iterable<B> b;
    public final F3<T, A, B, R> f;
    public Yield<A> buildYield(Yield<R> yield) {
        ERROR: Unknown local ai : Iterator#T<§A#G§> = <Zip3Link#C<T#G, A#G, B#G, R#G>>self.<eIU>a\Iterable#T<§A#G§>\.<dIa>iterator\Iterator#T<§A#G§>\;
        ERROR: Unknown local bi : Iterator#T<§B#G§> = <Zip3Link#C<T#G, A#G, B#G, R#G>>self.<eIU>b\Iterable#T<§B#G§>\.<dIa>iterator\Iterator#T<§B#G§>\;
        return ERROR: Unknown <to>Yield\Yield#C.class\.<dIt>decorate(base = <l>yield\Yield#C<§R#G§>\, yield = item : §A#G§ -> int = {
    if((!(<l>ai\Iterator#T<§A#G§>\.<dIa>hasNext\bool\) || !(<l>bi\Iterator#T<§B#G§>\.<dIa>hasNext\bool\))) return 1
else return <l>yield\Yield#C<§R#G§>\.<dI>yield(item = <Zip3Link#C<T#G, A#G, B#G, R#G>>self.<eIU>f\(§T#G§, §A#G§, §B#G§) -> §R#G§\.<d>apply( = <l>item\§A#G§\,  = <l>ai\Iterator#T<§A#G§>\.<dIa>next\§A#G§\,  = <l>bi\Iterator#T<§B#G§>\.<dIa>next\§B#G§\)\§R#G§\)\int\
})\Yield#C<§A#G§>\;
    }
    public Zip3Link(Iterable<A> a,Iterable<B> b,F3<T, A, B, R> f) {
    }
    static final ClassType<Zip3Link<T, A, B, R>> type;
}