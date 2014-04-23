package core.chain;

public class ArrayBuilder<T> extends Builder_impl<T, ImArray<T>> {
    private final MArray<T> array = new MArray<T>();
    @Override
    public void appendItem(T item) {
        this.array.appendItem(item);
    }
    @Override
    public ImArray<T> build() {
        return this.array.im();
    }
    public ArrayBuilder() {
    }
}