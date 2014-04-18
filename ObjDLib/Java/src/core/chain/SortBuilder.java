package core.chain;

public class SortBuilder<A> {
    public final Chain<A> chain;
    private final MArray<F2<A, A, Integer>> functions = new MArray<F2<A, A, Integer>>();
    public SortBuilder<A> ascBy(F<A, B> by) {
        functions.appendItem(ERROR: Unknown x : A#G, y : A#G -> int = return <l>by\§A#G§ -> B#G\.<d>apply( = <l>x\§A#G§\)\B#G\.<rdI>compare(to = <l>by\§A#G§ -> B#G\.<d>apply( = <l>y\§A#G§\)\B#G\)\int\);
        return ERROR: Unknown <SortBuilder#C<A#G>>self;
    }
    public SortBuilder<A> descBy(F<A, B> by) {
        functions.appendItem(ERROR: Unknown x : A#G, y : A#G -> int = return <l>by\§A#G§ -> B#G\.<d>apply( = <l>y\§A#G§\)\B#G\.<rdI>compare(to = <l>by\§A#G§ -> B#G\.<d>apply( = <l>x\§A#G§\)\B#G\)\int\);
        return ERROR: Unknown <SortBuilder#C<A#G>>self;
    }
    public SortBuilder<A> andF(F2<A, A, Integer> f) {
        functions.appendItem(f);
        return ERROR: Unknown <SortBuilder#C<A#G>>self;
    }
    public Chain<A> endSort() {
        return chain.sort(ERROR: Unknown x : §A#G§, y : §A#G§ -> int = {
    local var ret : int = 0
    local i : Iterator#T<§^(A#G, A#G) -> int§> = <SortBuilder#C<A#G>>self.<ep>functions\MArray#C<§^(A#G, A#G) -> int§>\.<rdIa>iterator\Iterator#T<§^(A#G, A#G) -> int§>\
    while(((<lm>ret\int\ == 0) && <l>i\Iterator#T<§^(A#G, A#G) -> int§>\.<dIa>hasNext\bool\)) {
    local f : (A#G, A#G) -> int = <l>i\Iterator#T<§^(A#G, A#G) -> int§>\.<dIa>next\§^(A#G, A#G) -> int§\
    (<lm>ret\int\ = <l>f\(§A#G§, §A#G§) -> int\.<d>apply( = <l>x\§A#G§\,  = <l>y\§A#G§\)\int\)
}
    return <lm>ret\int\
});
    }
    public SortBuilder(Chain<A> chain) {
    }
    static final ClassType<SortBuilder<A>> type;
}