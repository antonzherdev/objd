package core.chain;

public class MQueue<T> implements Queue<T> {
    private ImQueue<T> _queue = ImQueue().apply<T>();
    public void enqueueItem(T item) {
        this._queue = this._queue.addItem(item);
    }
    public T dequeue() {
        Tuple2<T, ImQueue<T>> p = this._queue.dequeue();
        this._queue = p.b;
        return p.a;
    }
    public int count() {
        return this._queue.count();
    }
    public MQueue() {
    }
}