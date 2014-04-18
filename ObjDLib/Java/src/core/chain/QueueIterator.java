package core.chain;

public class QueueIterator<T> implements Iterator<T> {
    public final ImList<T> in;
    public final ImList<T> out;
    private Iterator<T> i = in.iterator();
    private boolean isIn = ERROR: Unknown True;
    public boolean hasNext() {
        ERROR: Unknown if(<QueueIterator#C<T#G>>self.<emp>i\Iterator#T<§T#G§>\.<dIa>hasNext\bool\) return True
else if(<QueueIterator#C<T#G>>self.<emp>isIn\bool\) {
    (<QueueIterator#C<T#G>>self.<emp>isIn\bool\ = False)
    (<QueueIterator#C<T#G>>self.<emp>i\Iterator#T<§T#G§>\ = <QueueIterator#C<T#G>>self.<eIU>out\ImList#C<§T#G§>\.<dIa>reverse\ImList#C<§T#G§>\.<dIo>iterator\Iterator#T<§T#G§>\)
    return <QueueIterator#C<T#G>>self.<emp>i\Iterator#T<§T#G§>\.<dIa>hasNext\bool\
}
else return False;
    }
    public T next() {
        ERROR: Unknown if((!(<QueueIterator#C<T#G>>self.<emp>i\Iterator#T<§T#G§>\.<dIa>hasNext\bool\) && <QueueIterator#C<T#G>>self.<emp>isIn\bool\)) {
    (<QueueIterator#C<T#G>>self.<emp>isIn\bool\ = False)
    (<QueueIterator#C<T#G>>self.<emp>i\Iterator#T<§T#G§>\ = <QueueIterator#C<T#G>>self.<eIU>out\ImList#C<§T#G§>\.<dIa>reverse\ImList#C<§T#G§>\.<dIo>iterator\Iterator#T<§T#G§>\)
};
        return i.next();
    }
    public QueueIterator(ImList<T> in,ImList<T> out) {
    }
    static final ClassType<QueueIterator<T>> type;
}