package core.chain;

public interface MIterable<T> extends Iterable<T>, MTraversable<T> {
    MIterator<T> mutableIterator();
    boolean removeItem(T item);
    void mutableFilterBy(F<T, Boolean> by);
    ImIterable<T> im();
    ImIterable<T> imCopy();
}