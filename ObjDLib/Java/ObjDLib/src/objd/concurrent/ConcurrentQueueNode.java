package objd.concurrent;

public class ConcurrentQueueNode<T> {
    public T item;
    public ConcurrentQueueNode<T> next;
    public static <T> ConcurrentQueueNode<T> applyItem(final T item) {
        final ConcurrentQueueNode<T> ret = new ConcurrentQueueNode<T>();
        ret.item = item;
        return ret;
    }
    public ConcurrentQueueNode() {
    }
    public String toString() {
        return "ConcurrentQueueNode";
    }
}