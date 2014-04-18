package core.chain;

public class EmptyList<T> extends ImList<T> {
    public static EmptyList<Object> instance;
    public int count() {
    }
    public T head() {
    }
    public ImList<T> tail() {
    }
    public boolean isEmpty() {
    }
    public ImList<T> filterF(F<T, Boolean> f) {
    }
    public ImList<T> reverse() {
    }
    public void forEach(F<T, Void> each) {
    }
    public ImList<T> insertItem(C item) {
    }
    public EmptyList() {
    }
    static ClassType<EmptyList<T>> type;
}