package core.chain;

public class MTreeSet<T> extends TreeSet<T> implements MSet<T> {
    private static final Object obj = new Object();
    public final MTreeMap<T, Object> mmap;
    public static MTreeSet<T> applyComparator(F2<T, T, Integer> comparator) {
        return new MTreeSet<T>(new MTreeMap<T, Object>(comparator));
    }
    public static MTreeSet<T> apply() {
        return new MTreeSet<T>(MTreeMap().apply<T, Object>());
    }
    @Override
    public MIterator<T> mutableIterator() {
        return mmap.keys.mutableIterator();
    }
    @Override
    public void appendItem(T item) {
        mmap.setKeyValue(item, obj);
    }
    @Override
    public boolean removeItem(T item) {
        return ERROR: Unknown (<MTreeSet#C<T#G>>self.<eIU>mmap\MTreeMap#C<§T#G§, ^Object#C>\.<dIo>removeFor(key = <l>item\§T#G§\)\(^Object#C)?\ != none<^Object#C>);
    }
    @Override
    public void clear() {
        mmap.clear();
    }
    public void addAllObjects(Traversable<T> objects) {
        objects.forEach(new P<T>() {
            @Override
            public void f(T _) {
                appendItem(_);
            }
        });
    }
    public MTreeSet<T> reorder() {
        ERROR: Unknown local ret : MTreeSet#C<§T#G§> = <to>MTreeSet\MTreeSet#C.class\.<tcI>apply(mmap = <to>MTreeMap\MTreeMap#C.class\.<tcI>apply(comparator = <MTreeSet#C<T#G>>self.<eIU>mmap\MTreeMap#C<§T#G§, ^Object#C>\.<reIU>comparator\(§T#G§, §T#G§) -> int\)\MTreeMap#C<§T#G§, §^Object#C§>\)\MTreeSet#C<§T#G§>\;
        ret.addAllObjects(ERROR: Unknown <MTreeSet#C<T#G>>self);
        return ret;
    }
    @Override
    public ImTreeSet<T> im() {
        return new ImTreeSet<T>(mmap.im());
    }
    @Override
    public ImTreeSet<T> imCopy() {
        return new ImTreeSet<T>(mmap.imCopy());
    }
    public MTreeSet(MTreeMap<T, Object> mmap) {
    }
    public void mutableFilterBy(F<T, Boolean> by) {
        ERROR: Unknown local i : MIterator#T<§T#G§> = <MIterable#T<T#G>>self.<dIa>mutableIterator\MIterator#T<§T#G§>\;
        ERROR: Unknown while(<l>i\MIterator#T<§T#G§>\.<rdIa>hasNext\bool\) {
    if(<l>by\§T#G§ -> bool\.<d>apply( = <l>i\MIterator#T<§T#G§>\.<rdIa>next\§T#G§\)\bool\) <l>i\MIterator#T<§T#G§>\.<dIa>remove\void\
};
    }
}