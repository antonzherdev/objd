package core.chain;

public class IndexFunSeqIterator<T> extends Iterator_impl<T> {
    public final int count;
    public final F<Integer, T> f;
    private int i = ((int)0);
    @Override
    public boolean hasNext() {
        return this.i < this.count;
    }
    @Override
    public T next() {
        T ret = this.f.apply(this.i);
        this.i++;
        return ret;
    }
    public IndexFunSeqIterator(int count,F<Integer, T> f) {
        this.count = count;
        this.f = f;
    }
}