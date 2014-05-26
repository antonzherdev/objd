package objd.collection;

import objd.lang.F;
import objd.lang.P;

import java.util.ArrayList;

public abstract class Array<T> extends Seq_impl<T> {
    protected final ArrayList<T> list ;

    public Array() {
        this.list = new ArrayList<T>();
    }
    public Array(ArrayList<T> list) {
        this.list = list;
    }

    @Override
    public Iterator<T> iterator() {
        final java.util.Iterator<T> i = list.iterator();
        return new Iterator<T>() {
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
    public boolean isEmpty() {
        return list.isEmpty();
    }

    @Override
    public T head() {
        return list.isEmpty() ? null : list.get(0);
    }

    @Override
    public T applyIndex(int index) {
        return 0 > index || index >= list.size() ? null : list.get(index);
    }

    @Override
    public T last() {
        return list.isEmpty() ? null : list.get(list.size() - 1);
    }

    @Override
    public void forEach(P<T> each) {
        for (T t : list) {
            each.apply(t);
        }
    }

    @Override
    public Go goOn(F<T, Go> on) {
        for (T t : list) {
            if(on.apply(t) == Go.Break) return Go.Break;
        }
        return Go.Continue;
    }

    @Override
    public String toString() {
        return list.toString();
    }

    @Override
    public int hashCode() {
        return list.hashCode();
    }

    @Override
    public int count() {
        return list.size();
    }

    @Override
    public boolean containsItem(T item) {
        return list.contains(item);
    }

    @Override
    public T findWhere(F<T, Boolean> where) {
        for (T t : list) {
            if(where.apply(t)) return t;
        }
        return null;
    }

    @Override
    public boolean existsWhere(F<T, Boolean> where) {
        for (T t : list) {
            if(where.apply(t)) return true;
        }
        return false;
    }

    @Override
    public boolean allConfirm(F<T, Boolean> confirm) {
        for (T t : list) {
            if(!confirm.apply(t)) return false;
        }
        return true;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Array array = (Array) o;

        if (!list.equals(array.list)) return false;

        return true;
    }
}
