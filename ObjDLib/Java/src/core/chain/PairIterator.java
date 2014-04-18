package core.chain;

public class PairIterator<T> implements Iterator<T> {
    public final Pair<T> pair;
    private int state = ERROR: Unknown 0;
    public boolean hasNext() {
        return ERROR: Unknown (<PairIterator#C<T#G>>self.<emp>state\int\ < 2);
    }
    public T next() {
        ERROR: Unknown <PairIterator#C<T#G>>self.<emp>state\int\++;
        ERROR: Unknown if((<PairIterator#C<T#G>>self.<emp>state\int\ == 1)) return <PairIterator#C<T#G>>self.<eIU>pair\Pair#C<§T#G§>\.<eIU>a\§T#G§\
else return <PairIterator#C<T#G>>self.<eIU>pair\Pair#C<§T#G§>\.<eIU>b\§T#G§\;
    }
    public PairIterator(Pair<T> pair) {
    }
    static final ClassType<PairIterator<T>> type;
}