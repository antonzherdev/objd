package core.chain;

public class TreeSetBuilder<T> implements Builder<T, ImTreeSet<T>> {
    public final F2<T, T, Integer> comparator;
    private final MTreeSet<T> set = MTreeSet().applyComparator<T>(comparator);
    public static TreeSetBuilder<T> apply() {
        return new TreeSetBuilder<T>(new F2<T, T, Integer>() {
            @Override
            public Integer f(T a,T b) {
                ERROR: Unknown weak return <l>a\T#G\.<rdI>compare(to = <l>b\T#G\)\int\;
            }
        });
    }
    @Override
    public void appendItem(T item) {
        set.appendItem(item);
    }
    @Override
    public ImTreeSet<T> build() {
        return set.im();
    }
    public TreeSetBuilder(F2<T, T, Integer> comparator) {
    }
    public void appendAllItems(Traversable<T> items) {
        items.forEach(new P<T>() {
            @Override
            public void f(T _) {
                appendItem(_);
            }
        });
    }
}