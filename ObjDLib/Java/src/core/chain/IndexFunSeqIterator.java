package core.chain;

public class IndexFunSeqIterator<T> extends Iterator_impl<T> {
    public final int count;
    public final F<Integer, T> f;
    private int i;
    @Override
    public boolean hasNext() {
        return this.i < this.count;
    }
    @Override
    public T next() {
        final T ret = this.f.apply(this.i);
        this.i++;
        return ret;
    }
    public IndexFunSeqIterator(final int count, final F<Integer, T> f) {
        this.count = count;
        this.f = f;
        this.i = ((int)0);
    }
}