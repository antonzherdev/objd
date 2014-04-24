package core.chain;

public final class IndexFunSeq<T> extends ImSeq_impl<T> {
    public final int count;
    @Override
    public int count() {
        return count;
    }
    public final F<Integer, T> f;
    @Override
    public T applyIndex(int index) {
        if(index >= this.count) {
            return null;
        } else {
            return this.f.apply(index);
        }
    }
    @Override
    public Iterator<T> iterator() {
        return new IndexFunSeqIterator<T>(this.count, this.f);
    }
    public IndexFunSeq(int count,F<Integer, T> f) {
        this.count = count;
        this.f = f;
    }
}