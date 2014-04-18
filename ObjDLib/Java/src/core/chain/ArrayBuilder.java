package core.chain;

public class ArrayBuilder<T> implements Builder<T, ImArray<T>> {
    private final MArray<T> array = new MArray();
    public void appendItem(T item) {
        ERROR: Unknown <ArrayBuilder#C<T#G>>self.<ep>array\MArray#C<§T#G§>\.<rdIa>append(item = <l>item\§T#G§\)\void\;
    }
    public ImArray<T> build() {
        return ERROR: Unknown <ArrayBuilder#C<T#G>>self.<ep>array\MArray#C<§T#G§>\.<dIo>im\[§T#G§]\;
    }
    public ArrayBuilder() {
    }
    static final ClassType<ArrayBuilder<T>> type;
    public void appendAllItems(Traversable<T> items) {
        ERROR: Unknown <l>items\Traversable#T<§T#G§>\.<dI>for(each = _ : §T#G§ -> void = <Builder#T<T#G, C#G>>self.<dIa>append(item = <l>_\§T#G§\)\void\)\void\;
    }
}