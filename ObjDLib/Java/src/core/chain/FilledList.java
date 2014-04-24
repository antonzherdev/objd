package core.chain;

public final class FilledList<T> extends ImList<T> {
    public final T _head;
    public final ImList<T> tail;
    @Override
    public ImList<T> tail() {
        return tail;
    }
    public final int count = tail.count() + 1;
    @Override
    public int count() {
        return count;
    }
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
            return ((ImList<T>)new FilledList<T>(this._head, this.tail.filterF(f)));
        } else {
            return this.tail.filterF(f);
        }
    }
    @Override
    public ImList<T> reverse() {
        return reverseAndAddList(((ImList<T>)EmptyList.instance));
    }
    private ImList<T> reverseAndAddList(ImList<T> list) {
        FilledList<T> ret = new FilledList<T>(this._head, list);
        ImList<T> l = this.tail;
        while(!(l.isEmpty())) {
            ret = new FilledList<T>(((FilledList<T>)l)._head, ret);
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
                return ;
            }
            list = ((FilledList<T>)tail);
        }
    }
    @Override
    public  <C extends Comparable<C>> ImList<T> insertItem(C item) {
        ImList<T> before = ImList.<T>apply();
        FilledList<T> list = this;
        while(true) {
            T h = list._head;
            if(item.compareTo(((C)h)) < 0) {
                return new FilledList<T>(((T)item), before).reverseAndAddList(list);
            }
            before = ImList.<T>applyItemTail(h, before);
            if(list.tail.isEmpty()) {
                return new FilledList<T>(((T)item), before).reverse();
            }
            list = ((FilledList<T>)list.tail);
        }
        return list;
    }
    public FilledList(T _head,ImList<T> tail) {
        this._head = _head;
        this.tail = tail;
    }
}