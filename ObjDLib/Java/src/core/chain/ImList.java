package core.chain;

public class ImList<T> implements ImSeq<T> {
    public static ImList<T> apply() {
    }
    public static ImList<T> applyItem(T item) {
    }
    public static ImList<T> applyItemTail(T item,ImList<T> tail) {
    }
    public Iterator<T> iterator() {
    }
    public abstract ImList<T> tail();
    public abstract ImList<T> filterF(F<T, Boolean> f);
    public abstract ImList<T> reverse();
    public abstract ImList<T> insertItem(C item);
    public ImList() {
    }
    static ClassType<ImList<T>> type;
    public ImSeq<T> addItem(T item) {
    }
    public ImSeq<T> addSeq(Seq<T> seq) {
    }
    public ImSeq<T> subItem(T item) {
    }
    public MSeq<T> mCopy() {
    }
    public T applyIndex(int index) {
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