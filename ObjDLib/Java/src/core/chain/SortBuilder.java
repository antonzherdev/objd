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
                int ret = 0;
                Iterator<F2<A, A, Integer>> i = SortBuilder.this.functions.iterator();
                while(ret.equals(0) && i.hasNext()) {
                    F2<A, A, Integer> f = i.next();
                    ret = f.apply(x, y);
                }
                return ret;
            }
        });
    }
    public SortBuilder(Chain<A> chain) {
        this.chain = chain;
    }
}