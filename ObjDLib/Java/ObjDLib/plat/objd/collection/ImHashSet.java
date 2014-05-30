package objd.collection;


public class ImHashSet<T> extends HashSet<T> implements ImSet<T> {
    public ImHashSet() {
        super(new java.util.HashSet<T>());
    }

    public ImHashSet(java.util.HashSet<T> set) {
        super(set);
    }

    @Override
    public MHashSet<T> mCopy() {
        return new MHashSet<T>(new java.util.HashSet<T>(set));
    }
}
