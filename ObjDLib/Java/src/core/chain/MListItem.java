package core.chain;

public class MListItem<T> {
    public T data;
    public MListItem<T> next;
    public MListItem<T> prev;
    public MListItem(final T data) {
        this.data = data;
    }
}