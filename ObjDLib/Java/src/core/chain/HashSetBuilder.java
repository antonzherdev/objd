package core.chain;

public class HashSetBuilder<T> implements Builder<T, ImHashSet<T>> {
    public MHashSet<T> set;
    public void appendItem(T item) {
    }
    public ImHashSet<T> build() {
    }
    public HashSetBuilder() {
    }
    static ClassType<HashSetBuilder<T>> type;
    public void appendAllItems(Traversable<T> items) {
    }
}