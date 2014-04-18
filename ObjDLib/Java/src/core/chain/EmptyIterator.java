package core.chain;

public class EmptyIterator<T> implements Iterator<T> {
    public static final EmptyIterator<Object> instance = new EmptyIterator<Object>();
    public boolean hasNext() {
        return ERROR: Unknown False;
    }
    public T next() {
        ERROR: Unknown throw "Iterator is empty";
    }
    public EmptyIterator() {
    }
    static final ClassType<EmptyIterator<T>> type;
}