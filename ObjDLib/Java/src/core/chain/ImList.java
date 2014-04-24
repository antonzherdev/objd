package core.chain;

public abstract class ImList<T> extends ImSeq_impl<T> {
    public static  <T> ImList<T> apply() {
        return ((ImList<T>)EmptyList.instance);
    }
    public static  <T> ImList<T> applyItem(T item) {
        return new FilledList<T>(item, EmptyList.instance);
    }
    public static  <T> ImList<T> applyItemTail(T item,ImList<T> tail) {
        return new FilledList<T>(item, tail);
    }
    @Override
    public Iterator<T> iterator() {
        ListIterator<T> i = new ListIterator<T>();
        i.list = this;
        return i;
    }
    @Override
    public abstract ImList<T> tail();
    public abstract ImList<T> filterF(F<T, Boolean> f);
    public abstract ImList<T> reverse();
    public abstract  <C extends Comparable<C>> ImList<T> insertItem(C item);
    public ImList() {
    }
}