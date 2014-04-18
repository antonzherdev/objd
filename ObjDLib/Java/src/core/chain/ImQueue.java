package core.chain;

public class ImQueue<T> implements Queue<T> {
    private static ImQueue<Object> empty;
    public ImList<T> in;
    public ImList<T> out;
    public static ImQueue<T> apply() {
    }
    public Iterator<T> iterator() {
    }
    public boolean isEmpty() {
    }
    public int count() {
    }
    public ImQueue<T> addItem(T item) {
    }
    public ImQueue<T> enqueueItem(T item) {
    }
    public Tuple2<T, ImQueue<T>> dequeue() {
    }
    public ImQueue(ImList<T> in,ImList<T> out) {
    }
    static ClassType<ImQueue<T>> type;
}