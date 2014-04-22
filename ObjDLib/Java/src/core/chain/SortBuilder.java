package core.chain;

public class SortBuilder<A> {
    public final Chain<A> chain;
    private final MArray<F2<A, A, Integer>> functions = new MArray<F2<A, A, Integer>>();
    public SortBuilder<A> ascBy(F<A, B> by) {
        this.functions.appendItem(new F2<A, A, Integer>() {
            @Override
            public Integer apply(A x,A y) {
                return by.apply(x).compareTo(by.apply(y));
            }
        });
        return this;
    }
    public SortBuilder<A> descBy(F<A, B> by) {
        this.functions.appendItem(new F2<A, A, Integer>() {
            @Override
            public Integer apply(A x,A y) {
                return by.apply(y).compareTo(by.apply(x));
            }
        });
        return this;
    }
    public SortBuilder<A> andF(F2<A, A, Integer> f) {
        this.functions.appendItem(f);
        return this;
    }
    public Chain<A> endSort() {
        return this.chain.sort(new F2<A, A, Integer>() {
            @Override
            public Integer apply(A x,A y) {
                int ret = ERROR: Unknown 0;
                Iterator<F2<A, A, Integer>> i = SortBuilder.this.functions.iterator();
                ERROR: Unknown while(((<lm>ret\int\ == 0) && <l>i\Iterator#T<§^(A#G, A#G) -> int§>\.<dIa>hasNext\bool\)) {
    local f : (A#G, A#G) -> int = <l>i\Iterator#T<§^(A#G, A#G) -> int§>\.<dIa>next\§^(A#G, A#G) -> int§\
    (<lm>ret\int\ = <l>f\(§A#G§, §A#G§) -> int\.<d>apply( = <l>x\§A#G§\,  = <l>y\§A#G§\)\int\)
};
                return ret;
            }
        });
    }
    public SortBuilder(Chain<A> chain) {
        this.chain = chain;
    }
}