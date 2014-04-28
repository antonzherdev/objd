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
        final T __tmp_0 = this.array.applyIndex(((int)this.i));
        if(__tmp_0 == null) {
            throw new RuntimeException("Not null");
        }
        final T ret = __tmp_0;
        this.i++;
        return ret;
    }
    public PArrayIterator(final PArray<T> array) {
        this.array = array;
        this.i = 0;
    }
}