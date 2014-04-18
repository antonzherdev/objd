package core.chain;

public class SortBuilder<A> {
    public Chain<A> chain;
    private MArray<F2<A, A, Integer>> functions;
    public SortBuilder<A> ascBy(F<A, B> by) {
    }
    public SortBuilder<A> descBy(F<A, B> by) {
    }
    public SortBuilder<A> andF(F2<A, A, Integer> f) {
    }
    public Chain<A> endSort() {
    }
    public SortBuilder(Chain<A> chain) {
    }
    static ClassType<SortBuilder<A>> type;
}