package objd.collection;

import objd.lang.*;

public class IndexFunSeq<T> extends ImSeq_impl<T> {
    public final int count;
    @Override
    public int count() {
        return count;
    }
    public final F<Integer, T> f;
    @Override
    public T applyIndex(final int index) {
        if(index >= this.count) {
            return null;
        } else {
            return ((T)(this.f.apply(index)));
        }
    }
    @Override
    public Iterator<T> iterator() {
        return new IndexFunSeqIterator<T>(this.count, this.f);
    }
    public IndexFunSeq(final int count, final F<Integer, T> f) {
        this.count = count;
        this.f = f;
    }
    public String toString() {
        return String.format("IndexFunSeq(%d)", this.count);
    }
}