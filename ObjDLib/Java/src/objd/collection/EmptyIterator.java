package objd.collection;

import objd.lang.*;

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
    }
    public String toString() {
        return "EmptyIterator";
    }
    static {
        instance = new EmptyIterator<Object>();
    }
}