package core.chain;

public class Range implements ImSeq<int> {
    public int start;
    public int end;
    public int step;
    public int count;
    public Integer applyIndex(int index) {
    }
    public Iterator<Integer> iterator() {
    }
    public Range setStep(int step) {
    }
    public boolean isEmpty() {
    }
    public static Range applyI(int i) {
    }
    public Range(int start,int end,int step) {
    }
    static ClassType<Range> type;
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
    public T head() {
    }
    public T last() {
    }
    public ImSeq<T> tail() {
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