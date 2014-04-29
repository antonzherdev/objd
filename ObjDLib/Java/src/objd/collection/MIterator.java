package objd.collection;

public interface MIterator<T> extends Iterator<T> {
    void remove();
    void setValue(final T value);
}