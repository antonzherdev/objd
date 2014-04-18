package core.chain;

public interface Iterator<T> {
    boolean hasNext();
    T next();
}