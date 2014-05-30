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
        if(this.item == null) {
            throw new NullPointerException();
        }
        final MListItem<T> p = this.item;
        this.item = p.next;
        this.prev = p;
        return p.data;
    }
    @Override
    public void remove() {
        if(this.prev == null) {
            throw new NullPointerException();
        }
        this.list.removeListItem(this.prev);
    }
    @Override
    public void setValue(final T value) {
        if(this.prev == null) {
            throw new NullPointerException();
        }
        this.prev.data = value;
    }
    public MListIterator(final MList<T> list) {
        this.list = list;
    }
    public String toString() {
        return String.format("MListIterator(%s)", this.list);
    }
}