package core.chain;

public class Zip3Link<T, A, B, R> implements ChainLink<T, R> {
    public final Iterable<A> a;
    public final Iterable<B> b;
    public final F3<T, A, B, R> f;
    @Override
    public Yield<A> buildYield(Yield<R> yield) {
        ERROR: Unknown local ai : Iterator#T<§A#G§> = <Zip3Link#C<T#G, A#G, B#G, R#G>>self.<eIU>a\Iterable#T<§A#G§>\.<dIa>iterator\Iterator#T<§A#G§>\;
        ERROR: Unknown local bi : Iterator#T<§B#G§> = <Zip3Link#C<T#G, A#G, B#G, R#G>>self.<eIU>b\Iterable#T<§B#G§>\.<dIa>iterator\Iterator#T<§B#G§>\;
        return Yield().decorateBaseYield<A>(yield, new F<A, Integer>() {
            @Override
            public Integer apply(A item) {
                if(ERROR: Unknown !(<l>ai\Iterator#T<§A#G§>\.<dIa>hasNext\bool\) || ERROR: Unknown !(<l>bi\Iterator#T<§B#G§>\.<dIa>hasNext\bool\)) {
                    return ERROR: Unknown 1;
                } else {
                    return yield.yieldItem(Zip3Link.this.f.apply(item, ai.next(), bi.next()));
                }
            }
        });
    }
    public Zip3Link(Iterable<A> a,Iterable<B> b,F3<T, A, B, R> f) {
    }
}