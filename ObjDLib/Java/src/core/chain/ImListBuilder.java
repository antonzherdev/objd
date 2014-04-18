package core.chain;

public class ImListBuilder<T> implements Builder<T, ImList<T>> {
    private ImList<T> list = ERROR: Unknown <to>ImList\ImList#C.class\.<dIt>apply\ImList#C<§T#G§>\;
    public void appendItem(T item) {
        ERROR: Unknown (<ImListBuilder#C<T#G>>self.<emp>list\ImList#C<§T#G§>\ = <to>ImList\ImList#C.class\.<dIt>apply(item = <l>item\§T#G§\, tail = <ImListBuilder#C<T#G>>self.<emp>list\ImList#C<§T#G§>\)\ImList#C<§T#G§>\);
    }
    public ImList<T> build() {
        return ERROR: Unknown <ImListBuilder#C<T#G>>self.<emp>list\ImList#C<§T#G§>\.<dIa>reverse\ImList#C<§T#G§>\;
    }
    public ImListBuilder() {
    }
    static final ClassType<ImListBuilder<T>> type;
    public void appendAllItems(Traversable<T> items) {
        ERROR: Unknown <l>items\Traversable#T<§T#G§>\.<dI>for(each = _ : §T#G§ -> void = <Builder#T<T#G, C#G>>self.<dIa>append(item = <l>_\§T#G§\)\void\)\void\;
    }
}