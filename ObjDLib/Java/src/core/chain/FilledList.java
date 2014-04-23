package core.chain;

public final class FilledList<T> extends ImList<T> {
    public final T _head;
    public final ImList<T> tail;
    public final int count = tail.count() + ERROR: Unknown 1;
    @Override
    public T head() {
        return this._head;
    }
    @Override
    public boolean isEmpty() {
        return ERROR: Unknown False;
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
        ERROR: Unknown while(!(<lm>l\ImList#C<§T#G§>\.<rdIo>isEmpty\bool\)) {
    (<lm>ret\FilledList#C<§T#G§>\ = <to>FilledList\FilledList#C.class\.<tcI>apply(_head = <lm>l\ImList#C<§T#G§>\.cast<FilledList#C<T#G>>.<eIU>_head\§T#G§\, tail = <lm>ret\FilledList#C<§T#G§>\)\FilledList#C<§T#G§>\)
    (<lm>l\ImList#C<§T#G§>\ = <lm>l\ImList#C<§T#G§>\.<dIoa>tail\ImList#C<§T#G§>\)
};
        return ret;
    }
    @Override
    public void forEach(P<T> each) {
        FilledList<T> list = this;
        ERROR: Unknown while(True) {
    <l>each\§T#G§ -> void\.<d>apply( = <lm>list\FilledList#C<§T#G§>\.<eIU>_head\§T#G§\)\void\
    local tail : ImList#C<§T#G§> = <lm>list\FilledList#C<§T#G§>\.<eIUo>tail\ImList#C<§T#G§>\
    if(<l>tail\ImList#C<§T#G§>\.<rdIo>isEmpty\bool\) return nil
    (<lm>list\FilledList#C<§T#G§>\ = <l>tail\ImList#C<§T#G§>\.cast<FilledList#C<T#G>>)
};
    }
    @Override
    public ImList<T> insertItem(C item) {
        ImList<T> before = ImList().apply<T>();
        FilledList<T> list = this;
        ERROR: Unknown while(True) {
    local h : T#G = <lm>list\FilledList#C<§T#G§>\.<eIU>_head\§T#G§\
    if((<l>item\C#G\.<rdIb>compare(to = <l>h\§T#G§\)\int\ < 0)) {
    return <to>FilledList\FilledList#C.class\.<tcI>apply(_head = <l>item\C#G\.cast<T#G>, tail = <lm>before\ImList#C<§T#G§>\)\FilledList#C<§T#G§>\.<dp>reverseAndAdd(list = <lm>list\FilledList#C<§T#G§>\)\ImList#C<§T#G§>\
}
    (<lm>before\ImList#C<§T#G§>\ = <to>ImList\ImList#C.class\.<dIt>apply(item = <l>h\§T#G§\, tail = <lm>before\ImList#C<§T#G§>\)\ImList#C<§T#G§>\)
    if(<lm>list\FilledList#C<§T#G§>\.<eIUo>tail\ImList#C<§T#G§>\.<rdIo>isEmpty\bool\) {
    return <to>FilledList\FilledList#C.class\.<tcI>apply(_head = <l>item\C#G\.cast<T#G>, tail = <lm>before\ImList#C<§T#G§>\)\FilledList#C<§T#G§>\.<dIo>reverse\ImList#C<§T#G§>\
}
    (<lm>list\FilledList#C<§T#G§>\ = <lm>list\FilledList#C<§T#G§>\.<eIUo>tail\ImList#C<§T#G§>\.cast<FilledList#C<T#G>>)
};
        return list;
    }
    public FilledList(T _head,ImList<T> tail) {
        this._head = _head;
        this.tail = tail;
    }
}