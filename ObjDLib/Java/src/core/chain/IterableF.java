package core.chain;

public class IterableF<T> implements ImIterable<T> {
    public F<Void, Iterator<T>> iteratorF;
    public Iterator<T> iterator() {
    }
    public IterableF(F<Void, Iterator<T>> iteratorF) {
    }
    static ClassType<IterableF<T>> type;
    public MIterable<T> mCopy() {
    }
    public int count() {
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