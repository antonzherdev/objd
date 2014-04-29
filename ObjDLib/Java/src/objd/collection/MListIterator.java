package objd.collection;

import objd.lang.*;

public class MListIterator<T> extends MIterator_impl<T> {
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
        }
        this.item = this.item.next;
        if(this.prev == null) {
            throw new RuntimeException("Not null");
        }
        return this.prev.data;
    }
    @Override
    public void remove() {
        if(this.prev == null) {
            throw new RuntimeException("Not null");
        }
        this.list.removeListItem(this.prev);
    }
    @Override
    public void setValue(final T value) {
        if(this.prev == null) {
            throw new RuntimeException("Not null");
        }
        this.prev.data = value;
    }
    public MListIterator(final MList<T> list) {
        this.list = list;
    }
}