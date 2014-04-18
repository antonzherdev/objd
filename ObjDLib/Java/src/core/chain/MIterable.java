package core.chain;

public interface MIterable<T> extends Iterable<T>, MTraversable<T> {
    MIterator<T> mutableIterator();
    @Override
    boolean removeItem(T item);
    void mutableFilterBy(F<T, Boolean> by);
    @Override
    ImIterable<T> im();
    @Override
    ImIterable<T> imCopy();
}