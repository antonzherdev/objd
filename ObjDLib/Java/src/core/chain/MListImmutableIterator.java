package core.chain;

public class MListImmutableIterator<T> implements Iterator<T> {
    public MListItem<T> item;
    @Override
    public boolean hasNext() {
        return ERROR: Unknown (<MListImmutableIterator#C<T#G>>self.<eImw>item\(^MListItem#C<§T#G§>)?\ != none<^MListItem#C<§T#G§>>);
    }
    @Override
    public T next() {
        ERROR: Unknown local r : (^MListItem#C<§T#G§>)? = <MListImmutableIterator#C<T#G>>self.<eImw>item\(^MListItem#C<§T#G§>)?\;
        ERROR: Unknown (<MListImmutableIterator#C<T#G>>self.<eImw>item\(^MListItem#C<§T#G§>)?\ = <MListImmutableIterator#C<T#G>>self.<eImw>item\(^MListItem#C<§T#G§>)?\.get.<eIm>next\(^MListItem#C<§T#G§>)?\);
        return ERROR: Unknown <l>r\(^MListItem#C<§T#G§>)?\.get.data;
    }
    public MListImmutableIterator() {
    }
}