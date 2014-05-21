package objd.chain;

import objd.lang.*;
import objd.collection.MHashMap;
import objd.collection.Go;

public class GroupByLink<T, K, V> extends ChainLink_impl<T, Tuple<K, V>> {
    public final F<T, K> by;
    public final F0<V> start;
    public final F2<V, T, V> fold;
    public final boolean mutableMode;
    public final float factor;
    @Override
    public Yield<T> buildYield(final Yield<Tuple<K, V>> yield) {
        final MHashMap<K, V> m = new MHashMap<K, V>();
        return Yield.<T, Tuple<K, V>>decorateBaseBeginYieldEnd(yield, new F<Integer, Go>() {
            @Override
            public Go apply(final Integer size) {
                return yield.beginYieldWithSize(((int)(size * GroupByLink.this.factor)));
            }
        }, new F<T, Go>() {
            @Override
            public Go apply(final T item) {
                final K k = GroupByLink.this.by.apply(item);
                final V v = m.applyKeyOrUpdateWith(k, GroupByLink.this.start);
                if(GroupByLink.this.mutableMode) {
                    GroupByLink.this.fold.apply(v, item);
                } else {
                    m.setKeyValue(k, GroupByLink.this.fold.apply(v, item));
                }
                return Go.Continue;
            }
        }, new F<Go, Go>() {
            @Override
            public Go apply(final Go result) {
                if(result.equals(Go.Break)) {
                    return yield.endYieldWithResult(result);
                } else {
                    return yield.yieldAllItems(m);
                }
            }
        });
    }
    public GroupByLink(final F<T, K> by, final F0<V> start, final F2<V, T, V> fold, final boolean mutableMode, final float factor) {
        this.by = by;
        this.start = start;
        this.fold = fold;
        this.mutableMode = mutableMode;
        this.factor = factor;
    }
}