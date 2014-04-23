package core.chain;

public abstract class ImList<T> extends ImSeq_impl<T> {
    public static ImList<T> apply() {
        return EmptyList().instance.ERROR: Unknown cast<ImList#C<T#G>>;
    }
    public static ImList<T> applyItem(T item) {
        return new FilledList<T>(item, EmptyList().instance);
    }
    public static ImList<T> applyItemTail(T item,ImList<T> tail) {
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
    public abstract ImList<T> insertItem(C item);
    public ImList() {
    }
}