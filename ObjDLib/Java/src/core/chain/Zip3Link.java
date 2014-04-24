package core.chain;

public class Zip3Link<T, A, B, R> implements ChainLink<T, R> {
    public final Iterable<A> a;
    public final Iterable<B> b;
    public final F3<T, A, B, R> f;
    @Override
    public Yield<A> buildYield(Yield<R> yield) {
        Iterator<A> ai = this.a.iterator();
        Iterator<B> bi = this.b.iterator();
        return Yield.<A>decorateBaseYield(yield, new F<A, Integer>() {
            @Override
            public Integer apply(A item) {
                if(ERROR: Unknown !(<l>ai\Iterator#T<§A#G§>\.<dIa>hasNext\bool\) || ERROR: Unknown !(<l>bi\Iterator#T<§B#G§>\.<dIa>hasNext\bool\)) {
                    return 1;
                } else {
                    return yield.yieldItem(Zip3Link.this.f.apply(item, ai.next(), bi.next()));
                }
            }
        });
    }
    public Zip3Link(Iterable<A> a,Iterable<B> b,F3<T, A, B, R> f) {
        this.a = a;
        this.b = b;
        this.f = f;
    }
}