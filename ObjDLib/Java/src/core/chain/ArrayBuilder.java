package core.chain;

public class ArrayBuilder<T> implements Builder<T, ImArray<T>> {
    private final MArray<T> array = new MArray<T>();
    public void appendItem(T item) {
        array.appendItem(item);
    }
    public ImArray<T> build() {
        return array.im();
    }
    public ArrayBuilder() {
    }
    static final ClassType<ArrayBuilder<T>> type;
    public void appendAllItems(Traversable<T> items) {
        items.forEach(ERROR: Unknown _ : §T#G§ -> void = <Builder#T<T#G, C#G>>self.<dIa>append(item = <l>_\§T#G§\)\void\);
    }
}