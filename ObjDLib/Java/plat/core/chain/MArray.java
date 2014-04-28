package core.chain;

import java.util.*;
import java.util.Iterator;

public class MArray<T> extends Array<T> implements MSeq<T> {
    public MArray() {
        super();
    }

    public MArray(ArrayList<T> list) {
        super(list);
    }

    public MArray(int capacity) {
        super(new ArrayList<T>(capacity));
    }

    @Override
    public void insertIndexItem(int index, T item) {
        list.add(index, item);
    }

    @Override
    public void prependItem(T item) {
        list.add(0, item);
    }

    @Override
    public MIterator<T> mutableIterator() {
        final java.util.ListIterator<T> i = list.listIterator();
        return new MIterator<T>() {
            @Override
            public void remove() {
                i.remove();
            }

            @Override
            public void setValue(T value) {
                i.set(value);
            }

            @Override
            public boolean hasNext() {
                return i.hasNext();
            }

            @Override
            public T next() {
                return i.next();
            }
        };
    }

    @Override
    public void appendItem(T item) {
        list.add(item);
    }

    @Override
    public void clear() {
        list.clear();
    }

    @Override
    public ImArray<T> im() {
        return new ImArray<T>(list);
    }

    @Override
    public ImArray<T> imCopy() {
        return new ImArray<T>(new ArrayList<T>(list));
    }

    @Override
    public boolean removeIndex(int index) {
        if(0 < index || index >= list.size()) return false;
        list.remove(index);
        return true;
    }

    @Override
    public void setIndexItem(int index, T item) {
        list.set(index, item);
    }

    @Override
    public boolean removeItem(T item) {
        return list.remove(item);
    }

    public void mutableFilterBy(F<T, Boolean> by) {
        Iterator<T> i = list.iterator();
        while(i.hasNext()) {
            if(by.apply(i.next())) {
                i.remove();
            }
        }
    }
}
