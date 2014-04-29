package objd.collection;

public class EmptyIterator<T> extends Iterator_impl<T> {
    public static final EmptyIterator<Object> instance;
    @Override
    public boolean hasNext() {
        return false;
    }
    @Override
    public T next() {
        throw new RuntimeException("Iterator is empty");
    }
    public EmptyIterator() {
        this.instance = new EmptyIterator<Object>();
    }
}