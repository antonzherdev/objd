package core.chain;

public class ImListBuilder<T> implements Builder<T, ImList<T>> {
    private ImList<T> list = ImList().apply<T>();
    @Override
    public void appendItem(T item) {
        ERROR: Unknown (<ImListBuilder#C<T#G>>self.<emp>list\ImList#C<§T#G§>\ = <to>ImList\ImList#C.class\.<dIt>apply(item = <l>item\§T#G§\, tail = <ImListBuilder#C<T#G>>self.<emp>list\ImList#C<§T#G§>\)\ImList#C<§T#G§>\);
    }
    @Override
    public ImList<T> build() {
        return list.reverse();
    }
    public ImListBuilder() {
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