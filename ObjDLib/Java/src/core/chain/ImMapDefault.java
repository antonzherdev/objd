package core.chain;

public class ImMapDefault<K, V> implements ImIterable<Tuple2<K, V>> {
    public ImMap<K, V> map;
    public F<K, V> defaultFunc;
    public int count() {
    }
    public Iterator<Tuple2<K, V>> iterator() {
    }
    public V applyKey(K key) {
    }
    public Iterable<K> keys() {
    }
    public Iterable<V> values() {
    }
    public boolean containsKey(K key) {
    }
    public boolean isEqualMap(Map<K, V> map) {
    }
    public boolean isEqualMapDefault(ImMapDefault<K, V> mapDefault) {
    }
    public int hash() {
    }
    public MMapDefault<K, V> mCopy() {
    }
    public ImMapDefault(ImMap<K, V> map,F<K, V> defaultFunc) {
    }
    static ClassType<ImMapDefault<K, V>> type;
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