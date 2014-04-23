package core.chain;

public class MList<T> extends MSeq_impl<T> {
    private int _count = ERROR: Unknown 0.cast<uint>;
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
        if(index.equals(ERROR: Unknown 0)) {
            prependItem(item);
        } else {
            if(index >= this._count) {
                appendItem(item);
            } else {
                MListItem<T> c = this.headItem;
                int i = index;
                ERROR: Unknown while(((<lm>c\(^MListItem#C<§T#G§>)?\ != none<^MListItem#C<§T#G§>>) && (<lm>i\uint\ > 0))) {
    (<lm>c\(^MListItem#C<§T#G§>)¿\ = <lm>c\(^MListItem#C<§T#G§>)¿\.<eIm>next\(^MListItem#C<§T#G§>)?\)
};
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
            this._count = ERROR: Unknown 1.cast<uint>;
        } else {
            i.next = ERROR: Unknown <MList#C<T#G>>self.<emp>headItem\(^MListItem#C<§T#G§>)¿\.cast<(^MListItem#C<§T#G§>)?>;
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
            this._count = ERROR: Unknown 1.cast<uint>;
        } else {
            i.prev = ERROR: Unknown <MList#C<T#G>>self.<emp>lastItem\(^MListItem#C<§T#G§>)¿\.cast<(^MListItem#C<§T#G§>)?>;
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
        ERROR: Unknown while((<lm>i\(^MListItem#C<§T#G§>)?\ != none<^MListItem#C<§T#G§>>)) {
    <l>each\§T#G§ -> void\.<d>apply( = <lm>i\(^MListItem#C<§T#G§>)¿\.<eIUm>data\§T#G§\)\void\
    (<lm>i\(^MListItem#C<§T#G§>)¿\ = <lm>i\(^MListItem#C<§T#G§>)¿\.<eIm>next\(^MListItem#C<§T#G§>)?\)
};
    }
    @Override
    public boolean goOn(F<T, Boolean> on) {
        MListItem<T> i = this.headItem;
        ERROR: Unknown while((<lm>i\(^MListItem#C<§T#G§>)?\ != none<^MListItem#C<§T#G§>>)) {
    if(!(<l>on\§T#G§ -> bool\.<d>apply( = <lm>i\(^MListItem#C<§T#G§>)¿\.<eIUm>data\§T#G§\)\bool\)) return False
    (<lm>i\(^MListItem#C<§T#G§>)¿\ = <lm>i\(^MListItem#C<§T#G§>)¿\.<eIm>next\(^MListItem#C<§T#G§>)?\)
};
        return ERROR: Unknown True;
    }
    @Override
    public void mutableFilterBy(F<T, Boolean> by) {
        MListItem<T> i = this.headItem;
        ERROR: Unknown while((<lm>i\(^MListItem#C<§T#G§>)?\ != none<^MListItem#C<§T#G§>>)) {
    if(!(<l>by\§T#G§ -> bool\.<d>apply( = <lm>i\(^MListItem#C<§T#G§>)¿\.<eIUm>data\§T#G§\)\bool\)) <MList#C<T#G>>self.<dI>remove(listItem = <lm>i\(^MListItem#C<§T#G§>)¿\)\void\
    (<lm>i\(^MListItem#C<§T#G§>)¿\ = <lm>i\(^MListItem#C<§T#G§>)¿\.<eIm>next\(^MListItem#C<§T#G§>)?\)
};
    }
    @Override
    public T head() {
        return ERROR: Unknown <MList#C<T#G>>self.<emp>headItem\(^MListItem#C<§T#G§>)?\?.<eIUm>data\§T#G§\;
    }
    public MList() {
    }
}