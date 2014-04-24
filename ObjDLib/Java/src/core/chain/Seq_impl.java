package core.chain;

public abstract class Seq_impl<T> extends Iterable_impl<T> implements Seq<T> {
    @Override
    public boolean isEmpty() {
        return this.count().equals(0);
    }
    @Override
    public T head() {
        return applyIndex(((int)0));
    }
    public T applyIndex(int index) {
        if(index >= this.count()) {
            return null;
        }
        Iterator<T> i = this.iterator();
        int n = index;
        while(i.hasNext()) {
            if(n.equals(0)) {
                return i.next();
            }
            i.next();
            n--;
        }
        return null;
    }
    public Set<T> toSet() {
        return convertWithBuilder<Set<T>>(new HashSetBuilder<T>());
    }
    public boolean isEqualSeq(Seq<T> seq) {
        if(this.count().equals(seq.count())) {
            return false;
        }
        Iterator<T> ia = this.iterator();
        Iterator<T> ib = seq.iterator();
        while(ia.hasNext() && ib.hasNext()) {
            if(ia.next().equals(ib.next())) {
                return false;
            }
        }
        return true;
    }
    public T last() {
        return applyIndex(this.count() - 1);
    }
    public ImSeq<T> tail() {
        ArrayBuilder<T> builder = new ArrayBuilder<T>();
        Iterator<T> i = this.iterator();
        if(i.hasNext()) {
            i.next();
            while(i.hasNext()) {
                builder.appendItem(i.next());
            }
        }
        return builder.build();
    }
}