package objd.collection;

import objd.lang.*;

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
    public ArrayBuilder(final int capacity) {
        this.array = new MArray<T>(capacity);
    }
    static public <T> ArrayBuilder<T> apply() {
        return new ArrayBuilder<T>(((int)(0)));
    }
}