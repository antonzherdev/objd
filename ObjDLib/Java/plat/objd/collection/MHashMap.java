package objd.collection;

import objd.lang.F;
import objd.lang.F0;

import java.util.Map;

public class MHashMap<K, V> extends HashMap<K, V> implements MMap<K, V> {
    public MHashMap() {
        super(new java.util.HashMap<K,V>());
    }

    public MHashMap(java.util.HashMap<K, V> map) {
        super(map);
    }

    @Override
    public void setKeyValue(K key, V value) {
        map.put(key, value);
    }

    @Override
    public V removeKey(K key) {
        return map.remove(key);
    }

    @Override
    public V applyKeyOrUpdateWith(K key, F0<V> orUpdateWith) {
        if(map.containsKey(key)) return map.get(key);
        V v = orUpdateWith.apply();
        map.put(key, v);
        return v;
    }

    @Override
    public V modifyKeyBy(K key, F<V, V> by) {
        V v = by.apply(map.get(key));
        map.put(key, v);
        return v;
    }

    @Override
    public void appendItem(Tuple<K, V> item) {
        map.put(item.a, item.b);
    }

    @Override
    public MIterator<Tuple<K, V>> mutableIterator() {
        final java.util.Iterator<Map.Entry<K, V>> i = map.entrySet().iterator();
        return new MIterator<Tuple<K, V>>() {
            @Override
            public void remove() {
                i.remove();
            }

            @Override
            public void setValue(Tuple<K, V> value) {
                i.remove();
                map.put(value.a, value.b);
            }

            @Override
            public boolean hasNext() {
                return i.hasNext();
            }

            @Override
            public Tuple<K, V> next() {
                Map.Entry<K, V> e = i.next();
                return new Tuple<K, V>(e.getKey(), e.getValue());
            }
        };
    }

    @Override
    public boolean removeItem(Tuple<K, V> item) {
        return map.remove(item.a) != null;
    }

    @Override
    public void clear() {
        map.clear();
    }

    @Override
    public void mutableFilterBy(F<Tuple<K, V>, Boolean> by) {

    }

    @Override
    public ImHashMap<K, V> im() {
        return new ImHashMap<K, V>(map);
    }

    @Override
    public ImHashMap<K, V> imCopy() {
        return new ImHashMap<K, V>(new java.util.HashMap<K, V>(map));
    }

    @Override
    public void assignImMap(ImMap<K, V> imMap) {
        map.clear();
        if(imMap instanceof ImHashMap) {
            map.putAll(((ImHashMap<K, V>) imMap).map);
        } else {
            Iterator<Tuple<K, V>> i = imMap.iterator();
            while (i.hasNext()) {
                Tuple<K, V> t = i.next();
                map.put(t.a, t.b);
            }
        }
    }
}
