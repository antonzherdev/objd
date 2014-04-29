package objd.collection;

import objd.lang.Tuple;

import java.util.Map;

public abstract class HashMap<K, V> extends Map_impl<K, V> {
    protected final java.util.HashMap<K,V> map;

    protected HashMap(java.util.HashMap<K, V> map) {
        this.map = map;
    }

    @Override
    public V applyKey(K key) {
        return map.get(key);
    }

    @Override
    public boolean containsKey(K key) {
        return map.containsKey(key);
    }

    @Override
    public Iterable<K> keys() {
        return new HashMapIterable<K>() {
            @Override
            public Iterator<K> iterator() {
                final java.util.Iterator<K> i = map.keySet().iterator();
                return new Iterator<K>() {
                    @Override
                    public boolean hasNext() {
                        return i.hasNext();
                    }

                    @Override
                    public K next() {
                        return i.next();
                    }
                };
            }
        };
    }

    @Override
    public Iterable<V> values() {
        return new HashMapIterable<V>() {
            @Override
            public Iterator<V> iterator() {
                final java.util.Iterator<V> i = map.values().iterator();
                return new Iterator<V>() {
                    @Override
                    public boolean hasNext() {
                        return i.hasNext();
                    }

                    @Override
                    public V next() {
                        return i.next();
                    }
                };
            }
        };
    }

    @Override
    public Iterator<Tuple<K, V>> iterator() {
        final java.util.Iterator<Map.Entry<K, V>> i = map.entrySet().iterator();
        return new Iterator<Tuple<K, V>>() {
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

    public abstract class HashMapIterable<T> extends ImIterable_impl<T> {
        @Override
        public int count() {
            return map.size();
        }

        @Override
        public boolean isEmpty() {
            return map.isEmpty();
        }
    }
}
