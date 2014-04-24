package core.chain;

public class TreeSetBuilder<T> extends Builder_impl<T, ImTreeSet<T>> {
    public final F2<T, T, Integer> comparator;
    private final MTreeSet<T> set;
    public static  <T extends Comparable<T>> TreeSetBuilder<T> apply() {
        return new TreeSetBuilder<T>(new F2<T, T, Integer>() {
            @Override
            public Integer apply(T a,T b) {
                ERROR: Unknown weak return <l>a\T#G\.<rdIb>compare(to = <l>b\T#G\)\int\;
            }
        });
    }
    @Override
    public void appendItem(T item) {
        this.set.appendItem(item);
    }
    @Override
    public ImTreeSet<T> build() {
        return this.set.im();
    }
    public TreeSetBuilder(F2<T, T, Integer> comparator) {
        this.comparator = comparator;
        this.set = MTreeSet.<T>applyComparator(comparator);
    }
}