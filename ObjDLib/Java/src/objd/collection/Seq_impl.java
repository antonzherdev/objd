package objd.collection;

import objd.lang.*;

public abstract class Seq_impl<T> extends Iterable_impl<T> implements Seq<T> {
    public Seq_impl() {
    }
    @Override
    public boolean isEmpty() {
        return this.count() == 0;
    }
    @Override
    public T head() {
        return applyIndex(((int)(0)));
    }
    public T applyIndex(final int index) {
        if(index >= this.count()) {
            return null;
        }
        final Iterator<T> i = this.iterator();
        int n = index;
        while(i.hasNext()) {
            if(n == 0) {
                return i.next();
            }
            i.next();
            n--;
        }
        return null;
    }
    public Set<T> toSet() {
        return this.<ImHashSet<T>>convertWithBuilder(((Builder<T, ImHashSet<T>>)(((Builder)(HashSetBuilder.<T>apply())))));
    }
    public boolean isEqualSeq(final Seq<T> seq) {
        if(this.count() != seq.count()) {
            return false;
        }
        final Iterator<T> ia = this.iterator();
        final Iterator<T> ib = seq.iterator();
        while(ia.hasNext() && ib.hasNext()) {
            if(!(ia.next().equals(ib.next()))) {
                return false;
            }
        }
        return true;
    }
    public T last() {
        return applyIndex(this.count() - 1);
    }
    public ImSeq<T> tail() {
        final ArrayBuilder<T> builder = ArrayBuilder.<T>apply();
        final Iterator<T> i = this.iterator();
        if(i.hasNext()) {
            i.next();
            while(i.hasNext()) {
                builder.appendItem(i.next());
            }
        }
        return builder.build();
    }
}