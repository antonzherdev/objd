package core.chain;

public interface Iterable<T> extends Traversable<T> {
    int count();
    Iterator<T> iterator();
    @Override
    T head();
    boolean isEmpty();
    @Override
    void forEach(P<T> each);
    @Override
    void parForEach(P<T> each);
    @Override
    boolean goOn(F<T, Boolean> on);
    boolean containsItem(T item);
    @Override
    String toString();
    @Override
    int hashCode();
}