package core.chain;

public class QueueIterator<T> implements Iterator<T> {
    public final ImList<T> out;
    private Iterator<T> i = in.iterator();
    private boolean isIn = ERROR: Unknown True;
    @Override
    public boolean hasNext() {
        if(this.i.hasNext()) {
            return ERROR: Unknown True;
        } else {
            if(this.isIn) {
                this.isIn = ERROR: Unknown False;
                this.i = this.out.reverse().iterator();
                return this.i.hasNext();
            } else {
                return ERROR: Unknown False;
            }
        }
    }
    @Override
    public T next() {
        if(ERROR: Unknown !(<QueueIterator#C<T#G>>self.<emp>i\Iterator#T<§T#G§>\.<dIa>hasNext\bool\) && this.isIn) {
            this.isIn = ERROR: Unknown False;
            this.i = this.out.reverse().iterator();
        }
        return this.i.next();
    }
    public QueueIterator(ImList<T> in,ImList<T> out) {
        this.out = out;
    }
}