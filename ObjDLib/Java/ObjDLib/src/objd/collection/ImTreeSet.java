package objd.collection;

public class ImTreeSet<T> extends TreeSet<T> implements ImSet<T> {
    public final ImTreeMap<T, Object> immap;
    @Override
    public MTreeSet<T> mCopy() {
        return new MTreeSet<T>(this.immap.mCopy());
    }
    public ImTreeSet(final ImTreeMap<T, Object> immap) {
        super(immap);
        this.immap = immap;
    }
    public String toString() {
        return String.format("ImTreeSet(%s)", this.immap);
    }
}