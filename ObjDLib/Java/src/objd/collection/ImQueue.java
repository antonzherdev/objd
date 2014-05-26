package objd.collection;

import objd.lang.*;

public class ImQueue<T> extends Queue_impl<T> {
    private static final ImQueue<Object> empty;
    public final ImList<T> in;
    public final ImList<T> out;
    public static <T> ImQueue<T> apply() {
        return ((ImQueue<T>)(((ImQueue)(ImQueue.empty))));
    }
    public Iterator<T> iterator() {
        return new QueueIterator<T>(this.in, this.out);
    }
    public boolean isEmpty() {
        return this.in.isEmpty() && this.out.isEmpty();
    }
    public int count() {
        return this.in.count() + this.out.count();
    }
    public ImQueue<T> addItem(final T item) {
        if(this.isEmpty()) {
            return new ImQueue<T>(ImList.<T>apply(), ImList.<T>applyItem(item));
        } else {
            return new ImQueue<T>(ImList.<T>applyItemTail(item, this.in), this.out);
        }
    }
    public ImQueue<T> enqueueItem(final T item) {
        if(this.isEmpty()) {
            return new ImQueue<T>(ImList.<T>apply(), ImList.<T>applyItem(item));
        } else {
            return new ImQueue<T>(ImList.<T>applyItemTail(item, this.in), this.out);
        }
    }
    public Tuple<T, ImQueue<T>> dequeue() {
        if(!(this.out.isEmpty())) {
            return ((Tuple<T, ImQueue<T>>)(((Tuple)(new Tuple<T, ImQueue<T>>(this.out.head(), new ImQueue<T>(this.in, this.out.tail()))))));
        } else {
            if(this.in.isEmpty()) {
                return ((Tuple<T, ImQueue<T>>)(((Tuple)(new Tuple<T, ImQueue<T>>(null, this)))));
            } else {
                final ImList<T> rev = this.in.reverse();
                return ((Tuple<T, ImQueue<T>>)(((Tuple)(new Tuple<T, ImQueue<T>>(rev.head(), new ImQueue<T>(ImList.<T>apply(), rev.tail()))))));
            }
        }
    }
    public ImQueue(final ImList<T> in, final ImList<T> out) {
        this.in = in;
        this.out = out;
    }
    static {
        empty = new ImQueue<Object>(ImList.<Object>apply(), ImList.<Object>apply());
    }
}