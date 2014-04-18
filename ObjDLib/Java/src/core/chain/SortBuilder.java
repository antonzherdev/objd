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
                ERROR: Unknown local var ret : int = 0;
                ERROR: Unknown local i : Iterator#T<§^(A#G, A#G) -> int§> = <SortBuilder#C<A#G>>self.<ep>functions\MArray#C<§^(A#G, A#G) -> int§>\.<rdIa>iterator\Iterator#T<§^(A#G, A#G) -> int§>\;
                ERROR: Unknown while(((<lm>ret\int\ == 0) && <l>i\Iterator#T<§^(A#G, A#G) -> int§>\.<dIa>hasNext\bool\)) {
    local f : (A#G, A#G) -> int = <l>i\Iterator#T<§^(A#G, A#G) -> int§>\.<dIa>next\§^(A#G, A#G) -> int§\
    (<lm>ret\int\ = <l>f\(§A#G§, §A#G§) -> int\.<d>apply( = <l>x\§A#G§\,  = <l>y\§A#G§\)\int\)
};
                return ret;
            }
        });
    }
    public SortBuilder(Chain<A> chain) {
    }
}