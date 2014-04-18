package core.chain;

public class IndexFunSeq<T> implements ImSeq<T> {
    public int count;
    public F<Integer, T> f;
    public T applyIndex(int index) {
    }
    public Iterator<T> iterator() {
    }
    public IndexFunSeq(int count,F<Integer, T> f) {
    }
    static ClassType<IndexFunSeq<T>> type;
    public ImSeq<T> addItem(T item) {
    }
    public ImSeq<T> addSeq(Seq<T> seq) {
    }
    public ImSeq<T> subItem(T item) {
    }
    public MSeq<T> mCopy() {
    }
    public Set<T> toSet() {
    }
    public boolean isEqualSeq(Seq<T> seq) {
    }
    public boolean isEmpty() {
    }
    public T head() {
    }
    public T last() {
    }
    public ImSeq<T> tail() {
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
    public MIterable<T> mCopy() {
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