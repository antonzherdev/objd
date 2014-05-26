package objd.collection;

import objd.lang.*;

public class PairIterator<T> extends Iterator_impl<T> {
    public final Pair<T> pair;
    private int state;
    @Override
    public boolean hasNext() {
        return this.state < 2;
    }
    @Override
    public T next() {
        this.state++;
        if(this.state == 1) {
            return this.pair.a;
        } else {
            return this.pair.b;
        }
    }
    public PairIterator(final Pair<T> pair) {
        this.pair = pair;
        this.state = 0;
    }
    public String toString() {
        return String.format("PairIterator(%s)", this.pair);
    }
}