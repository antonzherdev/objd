package objd.chain;

import objd.lang.*;
import objd.collection.Traversable;
import objd.collection.Iterable;
import objd.collection.Go;

public class MulLink<A, B> extends ChainLink_impl<A, Tuple<A, B>> {
    private final Traversable<B> _collection;
    @Override
    public Yield<A> buildYield(final Yield<Tuple<A, B>> yield) {
        final Iterable<B> c = Util.<Iterable<B>>as(Iterable.class, this._collection);
        return Yield.<A, Tuple<A, B>>decorateBaseBeginYield(yield, ((c != null) ? (new F<Integer, Go>() {
            @Override
            public Go apply(final Integer size) {
                return yield.beginYieldWithSize(size * c.count());
            }
        }) : (null)), new F<A, Go>() {
            @Override
            public Go apply(final A a) {
                return MulLink.this._collection.goOn(new F<B, Go>() {
                    @Override
                    public Go apply(final B b) {
                        return yield.yieldItem(new Tuple<A, B>(a, b));
                    }
                });
            }
        });
    }
    public MulLink(final Traversable<B> collection) {
        this._collection = ((collection instanceof Chain) ? (((Traversable<B>)(((Traversable)(((Chain<B>)(((Chain)(collection)))).toArray()))))) : (collection));
    }
}