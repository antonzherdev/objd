package objd.chain;

import objd.lang.*;
import objd.collection.MArray;
import objd.collection.Iterator;

public class SortBuilder<A> {
    public final Chain<A> chain;
    private final MArray<F2<A, A, Integer>> functions;
    public <B extends Comparable<B>> SortBuilder<A> ascBy(final F<A, B> by) {
        this.functions.appendItem(new F2<A, A, Integer>() {
            @Override
            public Integer apply(final A x, final A y) {
                return by.apply(x).compareTo(by.apply(y));
            }
        });
        return this;
    }
    public <B extends Comparable<B>> SortBuilder<A> descBy(final F<A, B> by) {
        this.functions.appendItem(new F2<A, A, Integer>() {
            @Override
            public Integer apply(final A x, final A y) {
                return by.apply(y).compareTo(by.apply(x));
            }
        });
        return this;
    }
    public SortBuilder<A> andF(final F2<A, A, Integer> f) {
        this.functions.appendItem(f);
        return this;
    }
    public Chain<A> endSort() {
        return this.chain.sortComparator(new F2<A, A, Integer>() {
            @Override
            public Integer apply(final A x, final A y) {
                int ret = 0;
                final Iterator<F2<A, A, Integer>> i = SortBuilder.this.functions.iterator();
                while(ret == 0 && i.hasNext()) {
                    final F2<A, A, Integer> f = i.next();
                    ret = f.apply(x, y);
                }
                return ret;
            }
        });
    }
    public SortBuilder(final Chain<A> chain) {
        this.chain = chain;
        this.functions = new MArray<F2<A, A, Integer>>();
    }
    public String toString() {
        return String.format("SortBuilder(%s)", this.chain);
    }
}