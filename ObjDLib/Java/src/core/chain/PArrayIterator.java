package core.chain;

public class PArrayIterator<T> implements Iterator<T> {
    public final PArray<T> array;
    private int i = ERROR: Unknown 0;
    public boolean hasNext() {
        return ERROR: Unknown (<PArrayIterator#C<T#G>>self.<emp>i\int\ < <PArrayIterator#C<T#G>>self.<eIU>array\PArray#C<§T#G§>\.<eIUo>count\uint\);
    }
    public T next() {
        ERROR: Unknown local ret : T#G = <PArrayIterator#C<T#G>>self.<eIU>array\PArray#C<§T#G§>\.<dIo>apply(index = <PArrayIterator#C<T#G>>self.<emp>i\int\.cast<uint>)\(§T#G§)?\.get;
        ERROR: Unknown <PArrayIterator#C<T#G>>self.<emp>i\int\++;
        return ERROR: Unknown <l>ret\§T#G§\;
    }
    public PArrayIterator(PArray<T> array) {
    }
    static final ClassType<PArrayIterator<T>> type;
}