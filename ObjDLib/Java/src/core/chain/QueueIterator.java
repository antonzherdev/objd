package core.chain;

public class QueueIterator<T> implements Iterator<T> {
    public ImList<T> in;
    public ImList<T> out;
    private Iterator<T> i;
    private boolean isIn;
    public boolean hasNext() {
    }
    public T next() {
    }
    public QueueIterator(ImList<T> in,ImList<T> out) {
    }
    static ClassType<QueueIterator<T>> type;
}