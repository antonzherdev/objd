package core.chain;

public class MQueue<T> extends Queue_impl<T> {
    private ImQueue<T> _queue;
    public void enqueueItem(final T item) {
        this._queue = this._queue.addItem(item);
    }
    public T dequeue() {
        final Tuple<T, ImQueue<T>> p = this._queue.dequeue();
        this._queue = p.b;
        return p.a;
    }
    public int count() {
        return this._queue.count();
    }
    public MQueue() {
        this._queue = ImQueue.<T>apply();
    }
}