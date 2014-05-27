package objd.collection;

import objd.lang.*;

public abstract class Map_impl<K, V> extends Iterable_impl<Tuple<K, V>> implements Map<K, V> {
    public Map_impl() {
    }
    public boolean containsKey(final K key) {
        return applyKey(key) != null;
    }
    public boolean isValueEqualKeyValue(final K key, final V value) {
        final Boolean __tmp;
        {
            final V _ = applyKey(key);
            if(_ != null) {
                __tmp = _.equals(value);
            } else {
                __tmp = null;
            }
        }
        if(__tmp != null) {
            return __tmp;
        } else {
            return false;
        }
    }
}