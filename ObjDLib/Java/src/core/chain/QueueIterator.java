package core.chain;

public class QueueIterator<T> extends Iterator_impl<T> {
    public final ImList<T> out;
    private Iterator<T> i = in.iterator();
    private boolean isIn = true;
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
    public QueueIterator(ImList<T> in,ImList<T> out) {
        this.out = out;
    }
}