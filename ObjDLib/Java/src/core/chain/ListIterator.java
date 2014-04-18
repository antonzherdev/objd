package core.chain;

public class ListIterator<T> implements Iterator<T> {
    public ImList<T> list = EmptyList().instance.ERROR: Unknown cast<ImList#C<T#G>>;
    public boolean hasNext() {
        return ERROR: Unknown !(<ListIterator#C<T#G>>self.<eIm>list\ImList#C<§T#G§>\.<rdIo>isEmpty\bool\);
    }
    public T next() {
        ERROR: Unknown local ret : (§T#G§)? = <ListIterator#C<T#G>>self.<eIm>list\ImList#C<§T#G§>\.<rdIo>head\(§T#G§)?\;
        ERROR: Unknown (<ListIterator#C<T#G>>self.<eIm>list\ImList#C<§T#G§>\ = <ListIterator#C<T#G>>self.<eIm>list\ImList#C<§T#G§>\.<dIoa>tail\ImList#C<§T#G§>\);
        return ERROR: Unknown <l>ret\(§T#G§)?\.get;
    }
    public ListIterator() {
    }
    static final ClassType<ListIterator<T>> type;
}