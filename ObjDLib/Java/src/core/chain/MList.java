package core.chain;

public class MList<T> extends MSeq_impl<T> {
    private int _count = ((int)0);
    private MListItem<T> headItem;
    private MListItem<T> lastItem;
    @Override
    public int count() {
        return this._count;
    }
    @Override
    public Iterator<T> iterator() {
        MListImmutableIterator<T> i = new MListImmutableIterator<T>();
        i.item = this.headItem;
        return i;
    }
    @Override
    public MIterator<T> mutableIterator() {
        MListIterator<T> i = new MListIterator<T>(this);
        i.item = this.headItem;
        return i;
    }
    @Override
    public void insertIndexItem(int index,T item) {
        if(index.equals(0)) {
            prependItem(item);
        } else {
            if(index >= this._count) {
                appendItem(item);
            } else {
                MListItem<T> c = this.headItem;
                int i = index;
                while(c != null && i > 0) {
                    c = c.next;
                }
                if(c != null) {
                    MListItem<T> li = new MListItem<T>(item);
                    {
                        MListItem<T> __tmp_0_3_1 = c.next;
                        if(__tmp_0_3_1 != null) {
                            __tmp_0_3_1.prev = li;
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
    public void prependItem(T item) {
        MListItem<T> i = new MListItem<T>(item);
        if(this.headItem == null) {
            this.headItem = i;
            this.lastItem = i;
            this._count = ((int)1);
        } else {
            i.next = ((MListItem<T>)this.headItem);
            this.headItem.prev = i;
            this.headItem = i;
            ERROR: Unknown <MList#C<T#G>>self.<emp>_count\uint\++;
        }
    }
    @Override
    public void appendItem(T item) {
        MListItem<T> i = new MListItem<T>(item);
        if(this.lastItem == null) {
            this.headItem = i;
            this.lastItem = i;
            this._count = ((int)1);
        } else {
            i.prev = ((MListItem<T>)this.lastItem);
            this.lastItem.next = i;
            this.lastItem = i;
            ERROR: Unknown <MList#C<T#G>>self.<emp>_count\uint\++;
        }
    }
    public void removeListItem(MListItem<T> listItem) {
        if(this.headItem != null && this.headItem.equals(listItem)) {
            this.headItem = this.headItem.next;
            this.headItem.prev = null;
        } else {
            if(this.lastItem != null && this.lastItem.equals(listItem)) {
                this.lastItem = this.lastItem.prev;
                this.lastItem.next = null;
            } else {
                {
                    MListItem<T> __tmp_0_0 = listItem.prev;
                    if(__tmp_0_0 != null) {
                        __tmp_0_0.next = listItem.next;
                    }
                }
                {
                    MListItem<T> __tmp_0_1 = listItem.next;
                    if(__tmp_0_1 != null) {
                        __tmp_0_1.prev = listItem.prev;
                    }
                }
            }
        }
        ERROR: Unknown <MList#C<T#G>>self.<emp>_count\uint\--;
    }
    @Override
    public void clear() {
        this.headItem = null;
        this.lastItem = null;
    }
    public void removeHead() {
        {
            MListItem<T> _ = this.headItem;
            if(_ != null) {
                removeListItem(_);
            }
        }
    }
    public void removeLast() {
        {
            MListItem<T> _ = this.lastItem;
            if(_ != null) {
                removeListItem(_);
            }
        }
    }
    public T takeHead() {
        MListItem<T> h = this.headItem;
        if(h != null) {
            T r = h.data;
            removeListItem(h);
            return r;
        } else {
            return null;
        }
    }
    @Override
    public T last() {
        return ERROR: Unknown <MList#C<T#G>>self.<emp>lastItem\(^MListItem#C<§T#G§>)?\?.<eIUm>data\§T#G§\;
    }
    public T takeLast() {
        MListItem<T> h = this.lastItem;
        if(h != null) {
            T r = h.data;
            removeListItem(h);
            return r;
        } else {
            return null;
        }
    }
    @Override
    public void forEach(P<T> each) {
        MListItem<T> i = this.headItem;
        while(i != null) {
            each.apply(i.data);
            i = i.next;
        }
    }
    @Override
    public boolean goOn(F<T, Boolean> on) {
        MListItem<T> i = this.headItem;
        while(i != null) {
            if(ERROR: Unknown !(<l>on\§T#G§ -> bool\.<d>apply( = <lm>i\(^MListItem#C<§T#G§>)¿\.<eIUm>data\§T#G§\)\bool\)) {
                return false;
            }
            i = i.next;
        }
        return true;
    }
    @Override
    public void mutableFilterBy(F<T, Boolean> by) {
        MListItem<T> i = this.headItem;
        while(i != null) {
            if(ERROR: Unknown !(<l>by\§T#G§ -> bool\.<d>apply( = <lm>i\(^MListItem#C<§T#G§>)¿\.<eIUm>data\§T#G§\)\bool\)) {
                removeListItem(i);
            }
            i = i.next;
        }
    }
    @Override
    public T head() {
        return ERROR: Unknown <MList#C<T#G>>self.<emp>headItem\(^MListItem#C<§T#G§>)?\?.<eIUm>data\§T#G§\;
    }
    public MList() {
    }
}