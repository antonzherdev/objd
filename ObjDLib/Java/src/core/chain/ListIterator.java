package core.chain;

public class ListIterator<T> implements Iterator<T> {
    public ImList<T> list = EmptyList().instance.ERROR: Unknown cast<ImList#C<T#G>>;
    @Override
    public boolean hasNext() {
        return ERROR: Unknown !(<ListIterator#C<T#G>>self.<eIm>list\ImList#C<§T#G§>\.<rdIo>isEmpty\bool\);
    }
    @Override
    public T next() {
        ERROR: Unknown local ret : (§T#G§)? = <ListIterator#C<T#G>>self.<eIm>list\ImList#C<§T#G§>\.<rdIo>head\(§T#G§)?\;
        this.list = this.list.tail();
        if(ret == null) {
            throw new RuntimeException("Not null");
        } else {
            return ret;
        }
    }
    public ListIterator() {
    }
}