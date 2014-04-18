package core.chain;

public class MList<T> implements MSeq<T> {
    private int _count;
    private MListItem<T> headItem;
    private MListItem<T> lastItem;
    public int count() {
    }
    public Iterator<T> iterator() {
    }
    public MIterator<T> mutableIterator() {
    }
    public void insertIndexItem(int index,T item) {
    }
    public void prependItem(T item) {
    }
    public void appendItem(T item) {
    }
    public void removeListItem(MListItem<T> listItem) {
    }
    public void clear() {
    }
    public void removeHead() {
    }
    public void removeLast() {
    }
    public T takeHead() {
    }
    public T last() {
    }
    public T takeLast() {
    }
    public void forEach(F<T, Void> each) {
    }
    public boolean goOn(F<T, Boolean> on) {
    }
    public void mutableFilterBy(F<T, Boolean> by) {
    }
    public T head() {
    }
    public MList() {
    }
    static ClassType<MList<T>> type;
    public boolean removeIndex(int index) {
    }
    public void setIndexItem(int index,T item) {
    }
    public ImSeq<T> im() {
    }
    public ImSeq<T> imCopy() {
    }
    public T applyIndex(int index) {
    }
    public Set<T> toSet() {
    }
    public boolean isEqualSeq(Seq<T> seq) {
    }
    public boolean isEmpty() {
    }
    public ImSeq<T> tail() {
    }
    public boolean isEmpty() {
    }
    public void parForEach(F<T, Void> each) {
    }
    public boolean containsItem(T item) {
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
    public boolean removeItem(T item) {
    }
    public ImIterable<T> im() {
    }
    public ImIterable<T> imCopy() {
    }
    public boolean isEmpty() {
    }
    public void parForEach(F<T, Void> each) {
    }
    public boolean containsItem(T item) {
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
    public ImTraversable<T> im() {
    }
    public ImTraversable<T> imCopy() {
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