package core.chain;

public class TreeSetBuilder<T> extends Builder_impl<T, ImTreeSet<T>> {
    public final F2<T, T, Integer> comparator;
    private final MTreeSet<T> set;
    public static <T extends Comparable<T>> TreeSetBuilder<T> apply() {
        return new TreeSetBuilder<T>(new F2<T, T, Integer>() {
            @Override
            public Integer apply(final T a, final T b) {
                return a.compareTo(b);
            }
        });
    }
    @Override
    public void appendItem(final T item) {
        this.set.appendItem(item);
    }
    @Override
    public ImTreeSet<T> build() {
        return this.set.im();
    }
    public TreeSetBuilder(final F2<T, T, Integer> comparator) {
        this.comparator = comparator;
        this.set = MTreeSet.<T>applyComparator(comparator);
    }
}