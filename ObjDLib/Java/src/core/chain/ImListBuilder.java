package core.chain;

public class ImListBuilder<T> implements Builder<T, ImList<T>> {
    private ImList<T> list = ImList().apply<T>();
    public void appendItem(T item) {
        ERROR: Unknown (<ImListBuilder#C<T#G>>self.<emp>list\ImList#C<§T#G§>\ = <to>ImList\ImList#C.class\.<dIt>apply(item = <l>item\§T#G§\, tail = <ImListBuilder#C<T#G>>self.<emp>list\ImList#C<§T#G§>\)\ImList#C<§T#G§>\);
    }
    public ImList<T> build() {
        return list.reverse();
    }
    public ImListBuilder() {
    }
    static final ClassType<ImListBuilder<T>> type;
    public void appendAllItems(Traversable<T> items) {
        items.forEach(ERROR: Unknown _ : §T#G§ -> void = <Builder#T<T#G, C#G>>self.<dIa>append(item = <l>_\§T#G§\)\void\);
    }
}