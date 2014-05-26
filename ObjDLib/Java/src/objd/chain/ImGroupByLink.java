package objd.chain;

import objd.lang.*;
import objd.collection.MHashMap;
import objd.collection.Go;
import objd.collection.Traversable;

public class ImGroupByLink<T, K, V> extends ChainLink_impl<T, Tuple<K, V>> {
    public final double factor;
    public final F<T, K> by;
    public final F0<V> start;
    public final F2<V, T, V> fold;
    @Override
    public Yield<T> buildYield(final Yield<Tuple<K, V>> yield) {
        final MHashMap<K, V> m = new MHashMap<K, V>();
        return Yield.<T, Tuple<K, V>>decorateBaseBeginYieldEnd(yield, new F<Integer, Go>() {
            @Override
            public Go apply(final Integer _) {
                return Go.Continue;
            }
        }, new F<T, Go>() {
            @Override
            public Go apply(final T item) {
                final K k = ImGroupByLink.this.by.apply(item);
                final V v = m.applyKeyOrUpdateWith(k, ImGroupByLink.this.start);
                m.setKeyValue(k, ImGroupByLink.this.fold.apply(v, item));
                return Go.Continue;
            }
        }, new F<Go, Go>() {
            @Override
            public Go apply(final Go result) {
                if(result == Go.Break) {
                    return yield.endYieldWithResult(result);
                } else {
                    return yield.yieldAllItems(((Traversable<Tuple<K, V>>)(((Traversable)(m)))));
                }
            }
        });
    }
    public ImGroupByLink(final double factor, final F<T, K> by, final F0<V> start, final F2<V, T, V> fold) {
        this.factor = factor;
        this.by = by;
        this.start = start;
        this.fold = fold;
    }
    public String toString() {
        return String.format("ImGroupByLink(%f)", this.factor);
    }
}