package objd.collection;

import objd.lang.*;

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
            throw new NullPointerException();
        }
        return ret;
    }
    public ListIterator() {
        this.list = ((ImList<T>)(((ImList)(EmptyList.instance))));
    }
    public String toString() {
        return "ListIterator";
    }
}