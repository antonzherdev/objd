package core.chain;

public class MListImmutableIterator<T> implements Iterator<T> {
    public MListItem<T> item;
    public boolean hasNext() {
        return ERROR: Unknown (<MListImmutableIterator#C<T#G>>self.<eImw>item\(^MListItem#C<§T#G§>)?\ != none<^MListItem#C<§T#G§>>);
    }
    public T next() {
        ERROR: Unknown local r : (^MListItem#C<§T#G§>)? = <MListImmutableIterator#C<T#G>>self.<eImw>item\(^MListItem#C<§T#G§>)?\;
        ERROR: Unknown (<MListImmutableIterator#C<T#G>>self.<eImw>item\(^MListItem#C<§T#G§>)?\ = <MListImmutableIterator#C<T#G>>self.<eImw>item\(^MListItem#C<§T#G§>)?\.get.<eIm>next\(^MListItem#C<§T#G§>)?\);
        return ERROR: Unknown <l>r\(^MListItem#C<§T#G§>)?\.get.<eIUm>data\§T#G§\;
    }
    public MListImmutableIterator() {
    }
    static final ClassType<MListImmutableIterator<T>> type;
}