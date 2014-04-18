package core.chain;

public class ImTreeSet<T> extends TreeSet<T> implements ImSet<T> {
    public ImTreeMap<T, Object> immap;
    public MTreeSet<T> mCopy() {
    }
    public ImTreeSet(ImTreeMap<T, Object> immap) {
    }
    static ClassType<ImTreeSet<T>> type;
}