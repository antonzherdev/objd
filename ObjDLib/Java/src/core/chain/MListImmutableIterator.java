package core.chain;

public class MListImmutableIterator<T> implements Iterator<T> {
    public MListItem<T> item;
    @Override
    public boolean hasNext() {
        return this.item != null;
    }
    @Override
    public T next() {
        MListItem<T> r = this.item;
        if(this.item == null) {
            throw new RuntimeException("Not null");
        } else {
            this.item;
        }
        this.item = .next;
        if(r == null) {
            throw new RuntimeException("Not null");
        } else {
            r;
        }
        return .data;
    }
    public MListImmutableIterator() {
    }
}