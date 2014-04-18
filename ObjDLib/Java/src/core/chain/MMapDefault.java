package core.chain;

public class MMapDefault<K, V> implements MIterable<Tuple2<K, V>> {
    public MMap<K, V> map;
    public F<K, V> defaultFunc;
    public int count() {
    }
    public Iterator<Tuple2<K, V>> iterator() {
    }
    public MIterator<Tuple2<K, V>> mutableIterator() {
    }
    public V applyKey(K key) {
    }
    public Iterable<K> keys() {
    }
    public Iterable<V> values() {
    }
    public boolean containsKey(K key) {
    }
    public void setKeyValue(K key,V value) {
    }
    public V modifyKeyBy(K key,F<V, V> by) {
    }
    public void appendItem(Tuple2<K, V> item) {
    }
    public boolean removeItem(Tuple2<K, V> item) {
    }
    public void clear() {
    }
    public ImMapDefault<K, V> im() {
    }
    public ImMapDefault<K, V> imCopy() {
    }
    public MMapDefault(MMap<K, V> map,F<K, V> defaultFunc) {
    }
    static ClassType<MMapDefault<K, V>> type;
    public void mutableFilterBy(F<T, Boolean> by) {
    }
    public T head() {
    }
    public boolean isEmpty() {
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