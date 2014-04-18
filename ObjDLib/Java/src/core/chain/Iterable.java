package core.chain;

public interface Iterable<T> extends Traversable<T> {
    int count();
    Iterator<T> iterator();
    T head();
    boolean isEmpty();
    void forEach(F<T, Void> each);
    void parForEach(F<T, Void> each);
    boolean goOn(F<T, Boolean> on);
    boolean containsItem(T item);
    String description();
    int hash();
}