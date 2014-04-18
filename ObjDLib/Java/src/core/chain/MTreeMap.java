package core.chain;

public class MTreeMap<K, V> extends TreeMap<K, V> implements MMap<K, V> {
    private TreeMapEntry<K, V> _root;
    private int _size;
    public MTreeMapKeySet<K> keys;
    public static MTreeMap<K, V> apply() {
    }
    public ImTreeMap<K, V> imCopy() {
    }
    public ImTreeMap<K, V> im() {
    }
    public void assignImMap(ImMap<K, V> imMap) {
    }
    public TreeMapEntry<K, V> root() {
    }
    public int count() {
    }
    public void clear() {
    }
    public MIterator<Tuple2<K, V>> mutableIterator() {
    }
    public void setKeyValue(K key,V value) {
    }
    public V removeForKey(K key) {
    }
    private V deleteEntry(TreeMapEntry<K, V> entry) {
    }
    private void fixAfterInsertionEntry(TreeMapEntry<K, V> entry) {
    }
    private void fixAfterDeletionEntry(TreeMapEntry<K, V> entry) {
    }
    private void rotateLeftP(TreeMapEntry<K, V> p) {
    }
    private void rotateRightP(TreeMapEntry<K, V> p) {
    }
    public Tuple2<K, V> pollFirst() {
    }
    public MTreeMap(F2<K, K, Integer> comparator) {
    }
    static ClassType<MTreeMap<K, V>> type;
    public V objectForKeyOrUpdateWith(K key,F<Void, V> orUpdateWith) {
    }
    public V modifyKeyBy(K key,F<V, V> by) {
    }
    public V takeKey(K key) {
    }
    public void appendItem(Tuple2<K, V> item) {
    }
    public boolean removeItem(Tuple2<K, V> item) {
    }
    public boolean removeItem(T item) {
    }
    public void mutableFilterBy(F<T, Boolean> by) {
    }
}