package objd.chain;

import objd.lang.*;
import objd.collection.Traversable;
import objd.collection.Iterable;

public class MulLink<A, B> extends ChainLink_impl<A, Tuple<A, B>> {
    private final Traversable<T> _collection;
    @Override
    public Yield<A> buildYield(final Yield<Tuple<A, B>> yield) {
        final Iterable<B> c = this._collection.ERROR: Unknown as<Iterable#T<B#G>>;
        return Yield.<A, Tuple<A, B>>decorateBaseBeginYield(yield, ((c != null) ? (new F<Integer, Go>() {
            @Override
            public Go apply(final Integer size) {
                return yield.beginYieldWithSize(size * c.count());
            }
        }) : (null)), new F<A, Go>() {
            @Override
            public Go apply(final A a) {
                return MulLink.this._collection.goOn(new F<T, Go>() {
                    @Override
                    public Go apply(final T b) {
                        return yield.yieldItem(new Tuple<A, T>(a, b));
                    }
                });
            }
        });
    }
    public MulLink(final Traversable<B> collection) {
        this._collection = ((collection instanceof Chain) ? (((Traversable<T>)(((Chain<T>)(collection)).toArray()))) : (collection));
    }
}