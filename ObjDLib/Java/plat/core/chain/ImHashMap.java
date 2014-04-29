package core.chain;

public class ImHashMap<K, V> extends HashMap<K, V> implements ImMap<K, V> {
    public ImHashMap() {
        super(new java.util.HashMap<K,V>());
    }
    public ImHashMap(java.util.HashMap<K, V> map) {
        super(map);
    }

    @Override
    public ImMap<K, V> addItem(Tuple<K, V> item) {
        java.util.HashMap<K, V> m = new java.util.HashMap<K, V>(map);
        m.put(item.a, item.b);
        return new ImHashMap<K, V>(m);
    }

    @Override
    public MMap<K, V> mCopy() {
        return new MHashMap<K, V>(new java.util.HashMap<K, V>(map));
    }
}
