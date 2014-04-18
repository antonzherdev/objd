package core.chain;

public class EmptyIterator<T> implements Iterator<T> {
    public static final EmptyIterator<Object> instance = new EmptyIterator<Object>();
    @Override
    public boolean hasNext() {
        return ERROR: Unknown False;
    }
    @Override
    public T next() {
        ERROR: Unknown throw "Iterator is empty";
    }
    public EmptyIterator() {
    }
}