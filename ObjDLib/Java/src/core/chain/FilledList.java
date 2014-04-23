package core.chain;

public final class FilledList<T> extends ImList<T> {
    public final T _head;
    public final ImList<T> tail;
    public final int count = tail.count() + 1;
    @Override
    public T head() {
        return this._head;
    }
    @Override
    public boolean isEmpty() {
        return false;
    }
    @Override
    public ImList<T> filterF(F<T, Boolean> f) {
        if(f.apply(this._head)) {
            return ERROR: Unknown <to>FilledList\FilledList#C.class\.<tcI>apply(_head = <FilledList#C<T#G>>self.<eIU>_head\§T#G§\, tail = <FilledList#C<T#G>>self.<eIUo>tail\ImList#C<§T#G§>\.<dIa>filter(f = <l>f\§T#G§ -> bool\)\ImList#C<§T#G§>\)\FilledList#C<§T#G§>\.cast<ImList#C<§T#G§>>;
        } else {
            return this.tail.filterF(f);
        }
    }
    @Override
    public ImList<T> reverse() {
        return reverseAndAddList(EmptyList().instance.ERROR: Unknown cast<ImList#C<T#G>>);
    }
    private ImList<T> reverseAndAddList(ImList<T> list) {
        FilledList<T> ret = new FilledList<T>(this._head, list);
        ImList<T> l = this.tail;
        while(ERROR: Unknown !(<lm>l\ImList#C<§T#G§>\.<rdIo>isEmpty\bool\)) {
            ret = new FilledList<T>(l.ERROR: Unknown cast<FilledList#C<T#G>>._head, ret);
            l = l.tail();
        }
        return ret;
    }
    @Override
    public void forEach(P<T> each) {
        FilledList<T> list = this;
        while(true) {
            each.apply(list._head);
            ImList<T> tail = list.tail;
            if(tail.isEmpty()) {
                return null;
            }
            list = tail.ERROR: Unknown cast<FilledList#C<T#G>>;
        }
    }
    @Override
    public ImList<T> insertItem(C item) {
        ImList<T> before = ImList().apply<T>();
        FilledList<T> list = this;
        while(true) {
            T h = list._head;
            if(item.compareTo(h) < 0) {
                return new FilledList<T>(item.ERROR: Unknown cast<T#G>, before).reverseAndAddList(list);
            }
            before = ImList().applyItemTail<T>(h, before);
            if(list.tail.isEmpty()) {
                return new FilledList<T>(item.ERROR: Unknown cast<T#G>, before).reverse();
            }
            list = list.tail.ERROR: Unknown cast<FilledList#C<T#G>>;
        }
        return list;
    }
    public FilledList(T _head,ImList<T> tail) {
        this._head = _head;
        this.tail = tail;
    }
}