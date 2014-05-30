package objd.collection;

public class QueueIterator<T> extends Iterator_impl<T> {
    public final ImList<T> out;
    private Iterator<T> i;
    private boolean isIn;
    @Override
    public boolean hasNext() {
        if(this.i.hasNext()) {
            return true;
        } else {
            if(this.isIn) {
                this.isIn = false;
                this.i = this.out.reverse().iterator();
                return this.i.hasNext();
            } else {
                return false;
            }
        }
    }
    @Override
    public T next() {
        if(!(this.i.hasNext()) && this.isIn) {
            this.isIn = false;
            this.i = this.out.reverse().iterator();
        }
        return this.i.next();
    }
    public QueueIterator(final ImList<T> in, final ImList<T> out) {
        this.out = out;
        this.i = in.iterator();
        this.isIn = true;
    }
    public String toString() {
        return String.format("QueueIterator(%s)", this.out);
    }
}