package objd.collection;

import objd.lang.*;

public final class IndexFunSeq<T> extends ImSeq_impl<T> {
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
            return this.f.apply(index);
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
        return String.format("IndexFunSeq(%lu)", this.count);
    }
    public boolean equals(final Object to) {
        if(this == to) {
            return true;
        }
        if(to == null || !(to instanceof IndexFunSeq)) {
            return false;
        }
        final IndexFunSeq<T> o = ((IndexFunSeq<T>)(((IndexFunSeq)(to))));
        return this.count == o.count && this.f.equals(o.f);
    }
    public int hashCode() {
        int hash = 0;
        hash = hash * 31 + this.count;
        hash = hash * 31 + this.f.hashCode();
        return hash;
    }
}