package core.chain;

public interface Builder<T, C extends Traversable<T>> {
    void appendItem(T item);
    C build();
    void appendAllItems(Traversable<T> items);
}