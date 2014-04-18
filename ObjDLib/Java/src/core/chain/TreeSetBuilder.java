package core.chain;

public class TreeSetBuilder<T> implements Builder<T, ImTreeSet<T>> {
    public F2<T, T, Integer> comparator;
    private MTreeSet<T> set;
    public static TreeSetBuilder<T> apply() {
    }
    public void appendItem(T item) {
    }
    public ImTreeSet<T> build() {
    }
    public TreeSetBuilder(F2<T, T, Integer> comparator) {
    }
    static ClassType<TreeSetBuilder<T>> type;
    public void appendAllItems(Traversable<T> items) {
    }
}