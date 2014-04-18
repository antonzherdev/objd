package core.chain;

public class PArray<T> implements ImSeq<T> {
    public int stride;
    public F2<Pointer, Integer, T> wrap;
    public int count;
    public int length;
    public Pointer bytes;
    public boolean copied;
    public static PArray<T> applyStrideWrapCountCopyBytes(int stride,F2<Pointer, Integer, T> wrap,int count,Pointer copyBytes) {
    }
    public Iterator<T> iterator() {
    }
    public void dealloc() {
    }
    public T applyIndex(int index) {
    }
    public void forRefEach(F<Pointer, Void> each) {
    }
    public PArray(int stride,F2<Pointer, Integer, T> wrap,int count,int length,Pointer bytes,boolean copied) {
    }
    static ClassType<PArray<T>> type;
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