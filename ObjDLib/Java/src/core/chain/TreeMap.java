package core.chain;

public class TreeMap<K, V> implements ImMap<K, V> {
    public static int BLACK;
    public static int RED;
    public F2<K, K, Integer> comparator;
    public TreeMapValues<V> values;
    public V applyKey(K key) {
    }
    public V optKey(K key) {
    }
    public abstract TreeMapEntry<K, V> root();
    public boolean isEmpty() {
    }
    public TreeMapEntry<K, V> entryForKey(K key) {
    }
    public abstract TreeMapKeySet<K> keys();
    public Iterator<Tuple2<K, V>> iterator() {
    }
    public TreeMapIterator<K, V> iteratorHigherThanKey(K key) {
    }
    public TreeMapEntry<K, V> firstEntry() {
    }
    public K firstKey() {
    }
    public K lastKey() {
    }
    public K lowerKeyThanKey(K key) {
    }
    public K higherKeyThanKey(K key) {
    }
    private TreeMapEntry<K, V> lowerEntryThanKey(K key) {
    }
    private TreeMapEntry<K, V> higherEntryThanKey(K key) {
    }
    private TreeMapEntry<K, V> lastEntry() {
    }
    public TreeMap(F2<K, K, Integer> comparator) {
    }
    static ClassType<TreeMap<K, V>> type;
    public ImMap<K, V> addItem(Tuple2<K, V> item) {
    }
    public MMap<K, V> mCopy() {
    }
    public V getKeyOrValue(K key,V orValue) {
    }
    public boolean containsKey(K key) {
    }
    public boolean isValueEqualKeyValue(K key,V value) {
    }
    public int count() {
    }
    public T head() {
    }
    public void forEach(F<T, Void> each) {
    }
    public void parForEach(F<T, Void> each) {
    }
    public boolean goOn(F<T, Boolean> on) {
    }
    public boolean containsItem(T item) {
    }
    public void forEach(F<T, Void> each) {
    }
    public void parForEach(F<T, Void> each) {
    }
    public Chain<T> chain() {
    }
    public T findWhere(F<T, Boolean> where) {
    }
    public boolean existsWhere(F<T, Boolean> where) {
    }
    public boolean allConfirm(F<T, Boolean> confirm) {
    }
    public T head() {
    }
    public C convertWithBuilder(Builder<T, C> builder) {
    }
    public MIterable<T> mCopy() {
    }
    public int count() {
    }
    public T head() {
    }
    public void forEach(F<T, Void> each) {
    }
    public void parForEach(F<T, Void> each) {
    }
    public boolean goOn(F<T, Boolean> on) {
    }
    public boolean containsItem(T item) {
    }
    public void forEach(F<T, Void> each) {
    }
    public void parForEach(F<T, Void> each) {
    }
    public Chain<T> chain() {
    }
    public T findWhere(F<T, Boolean> where) {
    }
    public boolean existsWhere(F<T, Boolean> where) {
    }
    public boolean allConfirm(F<T, Boolean> confirm) {
    }
    public T head() {
    }
    public C convertWithBuilder(Builder<T, C> builder) {
    }
    public MTraversable<T> mCopy() {
    }
    public void forEach(F<T, Void> each) {
    }
    public void parForEach(F<T, Void> each) {
    }
    public Chain<T> chain() {
    }
    public T findWhere(F<T, Boolean> where) {
    }
    public boolean existsWhere(F<T, Boolean> where) {
    }
    public boolean allConfirm(F<T, Boolean> confirm) {
    }
    public T head() {
    }
    public C convertWithBuilder(Builder<T, C> builder) {
    }
}