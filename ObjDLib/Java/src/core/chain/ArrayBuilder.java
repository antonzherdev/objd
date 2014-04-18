package core.chain;

public class ArrayBuilder<T> implements Builder<T, ImArray<T>> {
    private final MArray<T> array = new MArray<T>();
    @Override
    public void appendItem(T item) {
        array.appendItem(item);
    }
    @Override
    public ImArray<T> build() {
        return array.im();
    }
    public ArrayBuilder() {
    }
    public void appendAllItems(Traversable<T> items) {
        items.forEach(new P<T>() {
            @Override
            public void f(T _) {
                appendItem(_);
            }
        });
    }
}