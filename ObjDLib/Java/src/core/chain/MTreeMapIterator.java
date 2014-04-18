package core.chain;

public class MTreeMapIterator<K, V> implements MIterator<Tuple2<K, V>> {
    public MTreeMap<K, V> map;
    private TreeMapEntry<K, V> prev;
    public TreeMapEntry<K, V> entry;
    public static MTreeMapIterator<K, V> applyMapEntry(MTreeMap<K, V> map,TreeMapEntry<K, V> entry) {
    }
    public boolean hasNext() {
    }
    public Tuple2<K, V> next() {
    }
    public void remove() {
    }
    public void setValue(Tuple2<K, V> value) {
    }
    public MTreeMapIterator(MTreeMap<K, V> map) {
    }
    static ClassType<MTreeMapIterator<K, V>> type;
}