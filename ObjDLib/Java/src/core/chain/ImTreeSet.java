package core.chain;

public class ImTreeSet<T> extends TreeSet<T> implements ImSet<T> {
    public final ImTreeMap<T, Object> immap;
    @Override
    public MTreeSet<T> mCopy() {
        return new MTreeSet<T>(this.immap.mCopy());
    }
    public ImTreeSet(ImTreeMap<T, Object> immap) {
        super(immap);
        this.immap = immap;
    }
}