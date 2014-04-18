package core.chain;

public interface MIterator<T> extends Iterator<T> {
    void remove();
    void setValue(T value);
}