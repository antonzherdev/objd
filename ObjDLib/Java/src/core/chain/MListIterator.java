package core.chain;

public class MListIterator<T> implements MIterator<T> {
    public final MList<T> list;
    private MListItem<T> prev;
    public MListItem<T> item;
    @Override
    public boolean hasNext() {
        return this.item != null;
    }
    @Override
    public T next() {
        this.prev = this.item;
        if(this.item == null) {
            throw new RuntimeException("Not null");
        } else {
            this.item;
        }
        this.item = .next;
        if(this.prev == null) {
            throw new RuntimeException("Not null");
        } else {
            this.prev;
        }
        return .data;
    }
    @Override
    public void remove() {
        if(this.prev == null) {
            throw new RuntimeException("Not null");
        } else {
            this.prev;
        }
        this.list.removeListItem();
    }
    @Override
    public void setValue(T value) {
        if(this.prev == null) {
            throw new RuntimeException("Not null");
        } else {
            this.prev;
        }
        .data = value;
    }
    public MListIterator(MList<T> list) {
        this.list = list;
    }
}