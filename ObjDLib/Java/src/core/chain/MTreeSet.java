package core.chain;

public class MTreeSet<T> extends TreeSet<T> implements MSet<T> {
    private static final Object obj;
    public final MTreeMap<T, Object> mmap;
    public static  <T> MTreeSet<T> applyComparator(F2<T, T, Integer> comparator) {
        return new MTreeSet<T>(new MTreeMap<T, Object>(comparator));
    }
    public static  <T> MTreeSet<T> apply() {
        return new MTreeSet<T>(MTreeMap.<T, Object>apply());
    }
    @Override
    public MIterator<T> mutableIterator() {
        return this.mmap.keys.mutableIterator();
    }
    @Override
    public void appendItem(T item) {
        this.mmap.setKeyValue(item, this.obj);
    }
    @Override
    public boolean removeItem(T item) {
        return this.mmap.removeForKey(item) != null;
    }
    @Override
    public void clear() {
        this.mmap.clear();
    }
    public void addAllObjects(Traversable<T> objects) {
        objects.forEach(new P<T>() {
            @Override
            public void apply(T _) {
                appendItem(_);
            }
        });
    }
    public MTreeSet<T> reorder() {
        MTreeSet<T> ret = new MTreeSet<T>(new MTreeMap<T, Object>(this.mmap.comparator));
        ret.addAllObjects(this);
        return ret;
    }
    @Override
    public ImTreeSet<T> im() {
        return new ImTreeSet<T>(this.mmap.im());
    }
    @Override
    public ImTreeSet<T> imCopy() {
        return new ImTreeSet<T>(this.mmap.imCopy());
    }
    public MTreeSet(MTreeMap<T, Object> mmap) {
        super(mmap);
        this.mmap = mmap;
        this.obj = new Object();
    }
    public void mutableFilterBy(F<T, Boolean> by) {
        MIterator<T> i = this.mutableIterator();
        while(i.hasNext()) {
            if(by.apply(i.next())) {
                i.remove();
            }
        }
    }
}