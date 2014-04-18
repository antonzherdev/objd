package core.chain;

public interface MTraversable<T> extends Traversable<T> {
    void appendItem(T item);
    boolean removeItem(T item);
    void clear();
    ImTraversable<T> im();
    ImTraversable<T> imCopy();
}