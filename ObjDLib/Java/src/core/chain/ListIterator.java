package core.chain;

public class ListIterator<T> extends Iterator_impl<T> {
    public ImList<T> list = ((ImList<T>)EmptyList.instance);
    @Override
    public boolean hasNext() {
        return !(this.list.isEmpty());
    }
    @Override
    public T next() {
        T ret = this.list.head();
        this.list = this.list.tail();
        if(ret == null) {
            throw new RuntimeException("Not null");
        } else {
            return ret;
        }
    }
    public ListIterator() {
    }
}