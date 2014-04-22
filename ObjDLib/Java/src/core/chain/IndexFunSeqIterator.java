package core.chain;

public class IndexFunSeqIterator<T> implements Iterator<T> {
    public final int count;
    public final F<Integer, T> f;
    private int i = ERROR: Unknown 0.cast<uint>;
    @Override
    public boolean hasNext() {
        return this.i < this.count;
    }
    @Override
    public T next() {
        T ret = this.f.apply(this.i);
        ERROR: Unknown <IndexFunSeqIterator#C<T#G>>self.<emp>i\uint\++;
        return ret;
    }
    public IndexFunSeqIterator(int count,F<Integer, T> f) {
        this.count = count;
        this.f = f;
    }
}