package objd.collection;

import objd.lang.*;

public abstract class ImList<T> extends ImSeq_impl<T> {
    @Override
    public abstract ImList<T> tail();
    public abstract ImList<T> filterF(final F<T, Boolean> f);
    public abstract ImList<T> reverse();
    public abstract <C extends Comparable<C>> ImList<C> insertItem(final C item);
    public static <T> ImList<T> apply() {
        return ((ImList<T>)(((ImList)(EmptyList.instance))));
    }
    public static <T> ImList<T> applyItem(final T item) {
        return new FilledList<T>(item, ((ImList<T>)(((ImList)(EmptyList.instance)))));
    }
    public static <T> ImList<T> applyItemTail(final T item, final ImList<T> tail) {
        return new FilledList<T>(item, tail);
    }
    @Override
    public Iterator<T> iterator() {
        final ListIterator<T> i = new ListIterator<T>();
        i.list = this;
        return i;
    }
    public ImList() {
    }
}