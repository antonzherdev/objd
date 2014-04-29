package objd.collection;

import objd.lang.*;

public class MListImmutableIterator<T> extends Iterator_impl<T> {
    public MListItem<T> item;
    @Override
    public boolean hasNext() {
        return this.item != null;
    }
    @Override
    public T next() {
        if(this.item == null) {
            throw new RuntimeException("Not null");
        }
        final MListItem<T> r = this.item;
        this.item = r.next;
        return r.data;
    }
    public MListImmutableIterator() {
    }
}