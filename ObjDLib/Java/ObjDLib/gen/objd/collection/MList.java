package objd.collection;

import objd.lang.*;

public class MList<T> extends MSeq_impl<T> {
    private int _count;
    private MListItem<T> headItem;
    private MListItem<T> lastItem;
    @Override
    public int count() {
        return this._count;
    }
    @Override
    public Iterator<T> iterator() {
        final MListImmutableIterator<T> i = new MListImmutableIterator<T>();
        i.item = this.headItem;
        return i;
    }
    @Override
    public MIterator<T> mutableIterator() {
        final MListIterator<T> i = new MListIterator<T>(this);
        i.item = this.headItem;
        return i;
    }
    @Override
    public void insertIndexItem(final int index, final T item) {
        if(index == 0) {
            prependItem(item);
        } else {
            if(index >= this._count) {
                appendItem(item);
            } else {
                MListItem<T> c = this.headItem;
                int i = index;
                while(c != null && i > 0) {
                    c = c.next;
                    i--;
                }
                if(c != null) {
                    final MListItem<T> li = new MListItem<T>(item);
                    {
                        final MListItem<T> __tmp_0ff_3t_1 = c.next;
                        if(__tmp_0ff_3t_1 != null) {
                            __tmp_0ff_3t_1.prev = li;
                        }
                    }
                    c.next = li;
                } else {
                    appendItem(item);
                }
            }
        }
    }
    @Override
    public void prependItem(final T item) {
        final MListItem<T> i = new MListItem<T>(item);
        if(this.headItem == null) {
            this.headItem = i;
            this.lastItem = i;
            this._count = ((int)(1));
        } else {
            i.next = ((MListItem<T>)(((MListItem)(this.headItem))));
            this.headItem.prev = i;
            this.headItem = i;
            this._count++;
        }
    }
    @Override
    public void appendItem(final T item) {
        final MListItem<T> i = new MListItem<T>(item);
        if(this.lastItem == null) {
            this.headItem = i;
            this.lastItem = i;
            this._count = ((int)(1));
        } else {
            i.prev = ((MListItem<T>)(((MListItem)(this.lastItem))));
            this.lastItem.next = i;
            this.lastItem = i;
            this._count++;
        }
    }
    public void removeListItem(final MListItem<T> listItem) {
        if(this.headItem != null && this.headItem.equals(listItem)) {
            this.headItem = this.headItem.next;
            this.headItem.prev = null;
        } else {
            if(this.lastItem != null && this.lastItem.equals(listItem)) {
                this.lastItem = this.lastItem.prev;
                this.lastItem.next = null;
            } else {
                {
                    final MListItem<T> __tmp_0ff_0 = listItem.prev;
                    if(__tmp_0ff_0 != null) {
                        __tmp_0ff_0.next = listItem.next;
                    }
                }
                {
                    final MListItem<T> __tmp_0ff_1 = listItem.next;
                    if(__tmp_0ff_1 != null) {
                        __tmp_0ff_1.prev = listItem.prev;
                    }
                }
            }
        }
        this._count--;
    }
    @Override
    public void clear() {
        this.headItem = null;
        this.lastItem = null;
    }
    public void removeHead() {
        {
            final MListItem<T> _ = this.headItem;
            if(_ != null) {
                removeListItem(((MListItem<T>)(((MListItem)(_)))));
            }
        }
    }
    public void removeLast() {
        {
            final MListItem<T> _ = this.lastItem;
            if(_ != null) {
                removeListItem(((MListItem<T>)(((MListItem)(_)))));
            }
        }
    }
    public T takeHead() {
        final MListItem<T> h = this.headItem;
        if(h != null) {
            final T r = h.data;
            removeListItem(((MListItem<T>)(((MListItem)(h)))));
            return r;
        } else {
            return null;
        }
    }
    @Override
    public T last() {
        return ((this.lastItem != null) ? (this.lastItem.data) : (null));
    }
    public T takeLast() {
        final MListItem<T> h = this.lastItem;
        if(h != null) {
            final T r = h.data;
            removeListItem(((MListItem<T>)(((MListItem)(h)))));
            return r;
        } else {
            return null;
        }
    }
    @Override
    public void forEach(final P<T> each) {
        MListItem<T> i = this.headItem;
        while(i != null) {
            each.apply(i.data);
            i = i.next;
        }
    }
    @Override
    public Go goOn(final F<T, Go> on) {
        MListItem<T> i = this.headItem;
        while(i != null) {
            if(on.apply(i.data) == Go.Break) {
                return Go.Break;
            }
            i = i.next;
        }
        return Go.Continue;
    }
    @Override
    public void mutableFilterBy(final F<T, Boolean> by) {
        MListItem<T> i = this.headItem;
        while(i != null) {
            if(!(by.apply(i.data))) {
                removeListItem(((MListItem<T>)(((MListItem)(i)))));
            }
            i = i.next;
        }
    }
    @Override
    public T head() {
        return ((this.headItem != null) ? (this.headItem.data) : (null));
    }
    public MList() {
        this._count = ((int)(0));
    }
    public String toString() {
        return "MList";
    }
}