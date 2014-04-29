package objd.collection;

public class ListIterator<T> extends Iterator_impl<T> {
    public ImList<T> list;
    @Override
    public boolean hasNext() {
        return !(this.list.isEmpty());
    }
    @Override
    public T next() {
        final T ret = this.list.head();
        this.list = this.list.tail();
        if(ret == null) {
            throw new RuntimeException("Not null");
        }
        return ret;
    }
    public ListIterator() {
        this.list = ((ImList<T>)EmptyList.instance);
    }
}