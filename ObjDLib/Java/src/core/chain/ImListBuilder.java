package core.chain;

public class ImListBuilder<T> extends Builder_impl<T, ImList<T>> {
    private ImList<T> list;
    @Override
    public void appendItem(T item) {
        this.list = ImList.<T>applyItemTail(item, this.list);
    }
    @Override
    public ImList<T> build() {
        return this.list.reverse();
    }
    public ImListBuilder() {
        this.list = ImList.<T>apply();
    }
}