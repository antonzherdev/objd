package objd.collection;

public interface Builder<T, C extends Traversable<T>> {
    void appendItem(final T item);
    C build();
    void appendAllItems(final Traversable<T> items);
}