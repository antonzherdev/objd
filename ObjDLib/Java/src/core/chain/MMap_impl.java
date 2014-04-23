package core.chain;

public abstract class MMap_impl<K, V> extends Map_impl<K, V> implements MMap<K, V> {
    @Override
    public void appendItem(Tuple2<K, V> item) {
        setKeyValue(item.b, item.a);
    }
    @Override
    public boolean removeItem(Tuple2<K, V> item) {
        return removeForKey(item.a) != null;
    }
    @Override
    public ImMap<K, V> im() {
        return this.imCopy();
    }
    @Override
    public ImMap<K, V> imCopy() {
        MHashMap<K, V> arr = new MHashMap<K, V>();
        forEach(new P<Tuple2<K, V>>() {
            @Override
            public void apply(Tuple2<K, V> item) {
                arr.setKeyValue(item.a, item.b);
            }
        });
        return arr.im();
    }
    public V objectForKeyOrUpdateWith(K key,F<Void, V> orUpdateWith) {
        V __tmp = optKey(key);
        if(__tmp != null) {
            return __tmp;
        } else {
            V init = ERROR: Unknown <l>orUpdateWith\void -> §V#G§\();
            setKeyValue(key, init);
            return init;
        }
    }
    public V modifyKeyBy(K key,F<V, V> by) {
        V newObject = by.apply(optKey(key));
        if(newObject == null) {
            removeForKey(key);
        } else {
            setKeyValue(key, newObject);
        }
        return newObject;
    }
    public V takeKey(K key) {
        V ret = optKey(key);
        removeForKey(key);
        return ret;
    }
    public void assignImMap(ImMap<K, V> imMap) {
        this.clear();
        imMap.forEach(new P<Tuple2<K, V>>() {
            @Override
            public void apply(Tuple2<K, V> _) {
                appendItem(_);
            }
        });
    }
    public void mutableFilterBy(F<T, Boolean> by) {
        MIterator<T> i = this.mutableIterator();
        ERROR: Unknown while(<l>i\MIterator#T<§T#G§>\.<rdIa>hasNext\bool\) {
    if(<l>by\§T#G§ -> bool\.<d>apply( = <l>i\MIterator#T<§T#G§>\.<rdIa>next\§T#G§\)\bool\) <l>i\MIterator#T<§T#G§>\.<dIa>remove\void\
};
    }
}