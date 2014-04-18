package core.chain;

public class MTreeMapKeyIterator<K> implements MIterator<K> {
    public MTreeMap<K, Object> map;
    private TreeMapEntry<K, Object> prev;
    public TreeMapEntry<K, Object> entry;
    public static MTreeMapKeyIterator<K> applyMapEntry(MTreeMap<K, Object> map,TreeMapEntry<K, Object> entry) {
    }
    public boolean hasNext() {
    }
    public K next() {
    }
    public void remove() {
    }
    public void setValue(K value) {
    }
    public MTreeMapKeyIterator(MTreeMap<K, Object> map) {
    }
    static ClassType<MTreeMapKeyIterator<K>> type;
}