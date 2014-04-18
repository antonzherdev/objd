package core.chain;

public class ArrayBuilder<T> implements Builder<T, ImArray<T>> {
    private MArray<T> array;
    public void appendItem(T item) {
    }
    public ImArray<T> build() {
    }
    public ArrayBuilder() {
    }
    static ClassType<ArrayBuilder<T>> type;
    public void appendAllItems(Traversable<T> items) {
    }
}