package core.chain;

public abstract class Map_impl<K, V> extends Iterable_impl<Tuple2<K, V>> implements Map<K, V> {
    public V getKeyOrValue(K key,V orValue) {
        V __tmp = optKey(key);
        if(__tmp != null) {
            return __tmp;
        } else {
            return orValue;
        }
    }
    public boolean containsKey(K key) {
        return optKey(key) != null;
    }
    public boolean isValueEqualKeyValue(K key,V value) {
        Boolean __tmp;
        {
            V _ = optKey(key);
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