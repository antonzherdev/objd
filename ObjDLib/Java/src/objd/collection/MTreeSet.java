package objd.collection;

import objd.lang.*;

public class MTreeSet<T> extends TreeSet<T> implements MSet<T> {
    private static final Object obj;
    public final MTreeMap<T, Object> mmap;
    public static <T> MTreeSet<T> applyComparator(final F2<T, T, Integer> comparator) {
        return new MTreeSet<T>(new MTreeMap<T, Object>(comparator));
    }
    public static <T extends Comparable<T>> MTreeSet<T> apply() {
        return new MTreeSet<T>(MTreeMap.<T, Object>apply());
    }
    @Override
    public MIterator<T> mutableIterator() {
        return this.mmap.keys.mutableIterator();
    }
    @Override
    public void appendItem(final T item) {
        this.mmap.setKeyValue(item, MTreeSet.obj);
    }
    @Override
    public boolean removeItem(final T item) {
        return this.mmap.removeKey(item) != null;
    }
    @Override
    public void clear() {
        this.mmap.clear();
    }
    public void addAllObjects(final Traversable<T> objects) {
        objects.forEach(new P<T>() {
            @Override
            public void apply(final T _) {
                appendItem(_);
            }
        });
    }
    public MTreeSet<T> reorder() {
        final MTreeSet<T> ret = new MTreeSet<T>(new MTreeMap<T, Object>(this.mmap.comparator));
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
    public MTreeSet(final MTreeMap<T, Object> mmap) {
        super(mmap);
        this.mmap = mmap;
    }
    public String toString() {
        return String.format("MTreeSet(%s)", this.mmap);
    }
    public void mutableFilterBy(final F<T, Boolean> by) {
        final MIterator<T> i = this.mutableIterator();
        while(i.hasNext()) {
            if(by.apply(i.next())) {
                i.remove();
            }
        }
    }
    static {
        obj = new Object();
    }
}