package core.chain;

public class PairIterator<T> implements Iterator<T> {
    public final Pair<T> pair;
    private int state = ERROR: Unknown 0;
    @Override
    public boolean hasNext() {
        return this.state < ERROR: Unknown 2;
    }
    @Override
    public T next() {
        ERROR: Unknown <PairIterator#C<T#G>>self.<emp>state\int\++;
        if(this.state.equals(ERROR: Unknown 1)) {
            return this.pair.a;
        } else {
            return this.pair.b;
        }
    }
    public PairIterator(Pair<T> pair) {
        this.pair = pair;
    }
}