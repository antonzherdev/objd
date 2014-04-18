package core.chain;

public class TreeSet<T> implements Set<T> {
    public TreeMap<T, Object> map;
    public T higherThanItem(T item) {
    }
    public T lowerThanItem(T item) {
    }
    public int count() {
    }
    public Iterator<T> iterator() {
    }
    public Iterator<T> iteratorHigherThanItem(T item) {
    }
    public T head() {
    }
    public T last() {
    }
    public boolean containsItem(T item) {
    }
    public TreeSet(TreeMap<T, Object> map) {
    }
    static ClassType<TreeSet<T>> type;
    public boolean isEmpty() {
    }
    public void forEach(F<T, Void> each) {
    }
    public void parForEach(F<T, Void> each) {
    }
    public boolean goOn(F<T, Boolean> on) {
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
    public C convertWithBuilder(Builder<T, C> builder) {
    }
}