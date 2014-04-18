package core.chain;

public class MapLink<A, B> implements ChainLink<A, B> {
    public final F<A, B> f;
    @Override
    public Yield<A> buildYield(Yield<B> yield) {
        return Yield().decorateBaseYield<A>(yield, new F<A, Integer>() {
            @Override
            public Integer f(A item) {
                return yield.yieldItem(f.apply(item));
            }
        });
    }
    public MapLink(F<A, B> f) {
    }
}