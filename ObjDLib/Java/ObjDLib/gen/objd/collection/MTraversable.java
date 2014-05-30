package objd.collection;

public interface MTraversable<T> extends Traversable<T> {
    void appendItem(final T item);
    boolean removeItem(final T item);
    void clear();
    ImTraversable<T> im();
    ImTraversable<T> imCopy();
    String toString();
}