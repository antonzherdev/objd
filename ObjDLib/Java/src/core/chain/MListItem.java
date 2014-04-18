package core.chain;

public class MListItem<T> {
    public T data;
    public MListItem<T> next;
    public MListItem<T> prev;
    public MListItem(T data) {
    }
    static ClassType<MListItem<T>> type;
}