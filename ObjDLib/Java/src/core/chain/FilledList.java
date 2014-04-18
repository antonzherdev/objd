package core.chain;

public class FilledList<T> extends ImList<T> {
    public T _head;
    public ImList<T> tail;
    public int count;
    public T head() {
    }
    public boolean isEmpty() {
    }
    public ImList<T> filterF(F<T, Boolean> f) {
    }
    public ImList<T> reverse() {
    }
    private ImList<T> reverseAndAddList(ImList<T> list) {
    }
    public void forEach(F<T, Void> each) {
    }
    public ImList<T> insertItem(C item) {
    }
    public FilledList(T _head,ImList<T> tail) {
    }
    static ClassType<FilledList<T>> type;
}