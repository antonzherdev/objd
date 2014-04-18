package core.chain;

public class MTreeSet<T> extends TreeSet<T> implements MSet<T> {
    private static Object obj;
    public MTreeMap<T, Object> mmap;
    public static MTreeSet<T> applyComparator(F2<T, T, Integer> comparator) {
    }
    public static MTreeSet<T> apply() {
    }
    public MIterator<T> mutableIterator() {
    }
    public void appendItem(T item) {
    }
    public boolean removeItem(T item) {
    }
    public void clear() {
    }
    public void addAllObjects(Traversable<T> objects) {
    }
    public MTreeSet<T> reorder() {
    }
    public ImTreeSet<T> im() {
    }
    public ImTreeSet<T> imCopy() {
    }
    public MTreeSet(MTreeMap<T, Object> mmap) {
    }
    static ClassType<MTreeSet<T>> type;
    public void mutableFilterBy(F<T, Boolean> by) {
    }
}