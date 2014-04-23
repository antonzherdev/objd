package core.chain;

public class EmptyIterator<T> extends Iterator_impl<T> {
    public static final EmptyIterator<Object> instance = new EmptyIterator<Object>();
    @Override
    public boolean hasNext() {
        return ERROR: Unknown False;
    }
    @Override
    public T next() {
        throw new RuntimeException("Iterator is empty");
    }
    public EmptyIterator() {
    }
}