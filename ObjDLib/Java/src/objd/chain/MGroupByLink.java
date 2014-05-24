package objd.chain;

import objd.lang.*;
import objd.collection.MHashMap;
import objd.collection.Go;
import objd.collection.Iterator;

public class MGroupByLink<T, K, V, W> extends ChainLink_impl<T, Tuple<K, W>> {
    public final double factor;
    public final F<T, K> by;
    public final F0<V> start;
    public final P2<V, T> append;
    public final F<V, W> finish;
    @Override
    public Yield<T> buildYield(final Yield<Tuple<K, W>> yield) {
        final MHashMap<K, V> m = new MHashMap<K, V>();
        return Yield.<T, Tuple<K, W>>decorateBaseBeginYieldEnd(yield, new F<Integer, Go>() {
            @Override
            public Go apply(final Integer size) {
                return yield.beginYieldWithSize(((int)(size * MGroupByLink.this.factor)));
            }
        }, new F<T, Go>() {
            @Override
            public Go apply(final T item) {
                final K k = MGroupByLink.this.by.apply(item);
                final V v = m.applyKeyOrUpdateWith(k, MGroupByLink.this.start);
                MGroupByLink.this.append.apply(v, item);
                return Go.Continue;
            }
        }, new F<Go, Go>() {
            @Override
            public Go apply(final Go result) {
                if(result == Go.Break) {
                    return yield.endYieldWithResult(result);
                } else {
                    Go __il__1r_0frp0ret = Go.Continue;
                    final Iterator<Tuple<K, V>> __il__1r_0frp0i = m.iterator();
                    while(__il__1r_0frp0i.hasNext()) {
                        final Tuple<K, V> t = __il__1r_0frp0i.next();
                        if(yield.yieldItem(new Tuple<K, W>(t.a, MGroupByLink.this.finish.apply(t.b))) == Go.Break) {
                            __il__1r_0frp0ret = Go.Break;
                            break;
                        }
                    }
                    return yield.endYieldWithResult(__il__1r_0frp0ret);
                }
            }
        });
    }
    public MGroupByLink(final double factor, final F<T, K> by, final F0<V> start, final P2<V, T> append, final F<V, W> finish) {
        this.factor = factor;
        this.by = by;
        this.start = start;
        this.append = append;
        this.finish = finish;
    }
}