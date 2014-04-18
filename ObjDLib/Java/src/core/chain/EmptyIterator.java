package core.chain;

public class EmptyIterator<T> implements Iterator<T> {
    public static EmptyIterator<Object> instance;
    public boolean hasNext() {
    }
    public T next() {
    }
    public EmptyIterator() {
    }
    static ClassType<EmptyIterator<T>> type;
}