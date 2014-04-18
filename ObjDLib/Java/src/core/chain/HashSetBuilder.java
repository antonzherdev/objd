package core.chain;

public class HashSetBuilder<T> implements Builder<T, ImHashSet<T>> {
    public final MHashSet<T> set = new MHashSet<T>();
    @Override
    public void appendItem(T item) {
        set.appendItem(item);
    }
    @Override
    public ImHashSet<T> build() {
        return set.im();
    }
    public HashSetBuilder() {
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