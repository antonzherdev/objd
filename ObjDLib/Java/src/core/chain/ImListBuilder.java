package core.chain;

public class ImListBuilder<T> implements Builder<T, ImList<T>> {
    private ImList<T> list;
    public void appendItem(T item) {
    }
    public ImList<T> build() {
    }
    public ImListBuilder() {
    }
    static ClassType<ImListBuilder<T>> type;
    public void appendAllItems(Traversable<T> items) {
    }
}