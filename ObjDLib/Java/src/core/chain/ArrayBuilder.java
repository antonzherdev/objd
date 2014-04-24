package core.chain;

public class ArrayBuilder<T> extends Builder_impl<T, ImArray<T>> {
    private final MArray<T> array;
    @Override
    public void appendItem(final T item) {
        this.array.appendItem(item);
    }
    @Override
    public ImArray<T> build() {
        return this.array.im();
    }
    public ArrayBuilder() {
        this.array = new MArray<T>();
    }
}