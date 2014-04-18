package core.chain;

public class MListIterator<T> implements MIterator<T> {
    public MList<T> list;
    private MListItem<T> prev;
    public MListItem<T> item;
    public boolean hasNext() {
    }
    public T next() {
    }
    public void remove() {
    }
    public void setValue(T value) {
    }
    public MListIterator(MList<T> list) {
    }
    static ClassType<MListIterator<T>> type;
}