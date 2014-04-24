package core.chain;

public abstract class Map_impl<K, V> extends Iterable_impl<Tuple<K, V>> implements Map<K, V> {
    public V getKeyOrValue(final K key, final V orValue) {
        final V __tmp = optKey(key);
        if(__tmp != null) {
            return __tmp;
        } else {
            return orValue;
        }
    }
    public boolean containsKey(final K key) {
        return optKey(key) != null;
    }
    public boolean isValueEqualKeyValue(final K key, final V value) {
        final Boolean __tmp;
        {
            final V _ = optKey(key);
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