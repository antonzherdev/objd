package core.chain;

public class ZipLink<T, A, R> implements ChainLink<T, R> {
    public final Iterable<A> a;
    public final F2<T, A, R> f;
    @Override
    public Yield<A> buildYield(Yield<R> yield) {
        ERROR: Unknown local ai : Iterator#T<§A#G§> = <ZipLink#C<T#G, A#G, R#G>>self.<eIU>a\Iterable#T<§A#G§>\.<dIa>iterator\Iterator#T<§A#G§>\;
        return Yield().decorateBaseYield<A>(yield, new F<A, Integer>() {
            @Override
            public Integer apply(A item) {
                if(ERROR: Unknown !(<l>ai\Iterator#T<§A#G§>\.<dIa>hasNext\bool\)) {
                    return ERROR: Unknown 1;
                } else {
                    return yield.yieldItem(ZipLink.this.f.apply(item, ai.next()));
                }
            }
        });
    }
    public ZipLink(Iterable<A> a,F2<T, A, R> f) {
        this.a = a;
        this.f = f;
    }
}