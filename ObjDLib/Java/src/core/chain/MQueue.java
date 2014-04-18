package core.chain;

public class MQueue<T> implements Queue<T> {
    private ImQueue<T> _queue = ImQueue().apply<T>();
    public void enqueueItem(T item) {
        this._queue = this._queue.addItem(item);
    }
    public T dequeue() {
        ERROR: Unknown local p : (^(§T#G§)?, ^ImQueue#C<§T#G§>) = <MQueue#C<T#G>>self.<emp>_queue\ImQueue#C<§T#G§>\.<dI>dequeue\(^(§T#G§)?, ^ImQueue#C<§T#G§>)\;
        this._queue = p.b;
        return p.a;
    }
    public int count() {
        return this._queue.count();
    }
    public MQueue() {
    }
}