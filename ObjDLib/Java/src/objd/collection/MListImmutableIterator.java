package objd.collection;

public class MListImmutableIterator<T> extends Iterator_impl<T> {
    public MListItem<T> item;
    @Override
    public boolean hasNext() {
        return this.item != null;
    }
    @Override
    public T next() {
        final MListItem<T> r = this.item;
        if(this.item == null) {
            throw new RuntimeException("Not null");
        }
        this.item = this.item.next;
        if(r == null) {
            throw new RuntimeException("Not null");
        }
        return r.data;
    }
    public MListImmutableIterator() {
    }
}