package core.chain;

public abstract class MMap_impl<K, V> extends Map_impl<K, V> implements MMap<K, V> {
    @Override
    public void appendItem(final Tuple<K, V> item) {
        setKeyValue(item.b, item.a);
    }
    @Override
    public boolean removeItem(final Tuple<K, V> item) {
        return removeForKey(item.a) != null;
    }
    @Override
    public ImMap<K, V> im() {
        return this.imCopy();
    }
    @Override
    public ImMap<K, V> imCopy() {
        final MHashMap<K, V> arr = new MHashMap<K, V>();
        {
            final Iterator<Tuple<K, V>> __inline__1_i = this.iterator();
            while(__inline__1_i.hasNext()) {
                final Tuple<K, V> item = __inline__1_i.next();
                arr.setKeyValue(item.a, item.b);
            }
        }
        return arr.im();
    }
    public V objectForKeyOrUpdateWith(final K key, final F0<V> orUpdateWith) {
        final V __tmp = optKey(key);
        if(__tmp != null) {
            return __tmp;
        } else {
            final V init = orUpdateWith.apply();
            setKeyValue(key, init);
            return init;
        }
    }
    public V modifyKeyBy(final K key, final F<V, V> by) {
        final V newObject = by.apply(optKey(key));
        if(newObject == null) {
            removeForKey(key);
        } else {
            setKeyValue(key, newObject);
        }
        return newObject;
    }
    public V takeKey(final K key) {
        final V ret = optKey(key);
        removeForKey(key);
        return ret;
    }
    public void assignImMap(final ImMap<K, V> imMap) {
        this.clear();
        {
            final Iterator<Tuple<K, V>> __inline__1_i = imMap.iterator();
            while(__inline__1_i.hasNext()) {
                final Tuple<K, V> _ = __inline__1_i.next();
                appendItem(_);
            }
        }
    }
    public void mutableFilterBy(final F<T, Boolean> by) {
        final MIterator<T> i = this.mutableIterator();
        while(i.hasNext()) {
            if(by.apply(i.next())) {
                i.remove();
            }
        }
    }
}