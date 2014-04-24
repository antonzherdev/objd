package core.chain;

public class PArrayIterator<T> extends Iterator_impl<T> {
    public final PArray<T> array;
    private int i;
    @Override
    public boolean hasNext() {
        return this.i < this.array.count;
    }
    @Override
    public T next() {
        final T ret = ERROR: Unknown {
    local __tmp_0 : (§T#G§)? = <PArrayIterator#C<T#G>>self.<eIU>array\PArray#C<§T#G§>\.<dIo>apply(index = <PArrayIterator#C<T#G>>self.<emp>i\int\.cast<uint>)\(§T#G§)?\
    if((<l>__tmp_0\(§T#G§)?\ == none<§T#G§>)) throw "Not null"
else <l>__tmp_0\(§T#G§)?\
};
        this.i++;
        return ret;
    }
    public PArrayIterator(final PArray<T> array) {
        this.array = array;
        this.i = 0;
    }
}