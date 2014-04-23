package core.chain;

public class MapOptLink<A, B> implements ChainLink<A, B> {
    public final F<A, B> f;
    @Override
    public Yield<A> buildYield(Yield<B> yield) {
        return Yield().decorateBaseYield<A>(yield, new F<A, Integer>() {
            @Override
            public Integer apply(A item) {
                Integer __tmp_0;
                {
                    B _ = MapOptLink.this.f.apply(item);
                    if(_ != null) {
                        __tmp_0 = yield.yieldItem(_);
                    } else {
                        __tmp_0 = null;
                    }
                }
                if(__tmp_0 != null) {
                    return __tmp_0;
                } else {
                    return 0;
                }
            }
        });
    }
    public MapOptLink(F<A, B> f) {
        this.f = f;
    }
}