package core.chain;

public class ImQueue<T> implements Queue<T> {
    private static final ImQueue<Object> empty = new ImQueue(ERROR: Unknown <to>ImList\ImList#C.class\.<dIt>apply\ImList#C<§^any§>\, ERROR: Unknown <to>ImList\ImList#C.class\.<dIt>apply\ImList#C<§^any§>\);
    public final ImList<T> in;
    public final ImList<T> out;
    public static ImQueue<T> apply() {
        return ERROR: Unknown <ImQueue#C.class>self.<ept>empty\ImQueue#C<^any>\;
    }
    public Iterator<T> iterator() {
        return new QueueIterator(ERROR: Unknown <ImQueue#C<T#G>>self.<eIU>in\ImList#C<§T#G§>\, ERROR: Unknown <ImQueue#C<T#G>>self.<eIU>out\ImList#C<§T#G§>\);
    }
    public boolean isEmpty() {
        return ERROR: Unknown (<ImQueue#C<T#G>>self.<eIU>in\ImList#C<§T#G§>\.<rdIo>isEmpty\bool\ && <ImQueue#C<T#G>>self.<eIU>out\ImList#C<§T#G§>\.<rdIo>isEmpty\bool\);
    }
    public int count() {
        return ERROR: Unknown (<ImQueue#C<T#G>>self.<eIU>in\ImList#C<§T#G§>\.<rdI>count\uint\ + <ImQueue#C<T#G>>self.<eIU>out\ImList#C<§T#G§>\.<rdI>count\uint\);
    }
    public ImQueue<T> addItem(T item) {
        ERROR: Unknown if(<ImQueue#C<T#G>>self.<dI>isEmpty\bool\) return <to>ImQueue\ImQueue#C.class\.<tcI>apply(in = <to>ImList\ImList#C.class\.<dIt>apply\ImList#C<§T#G§>\, out = <to>ImList\ImList#C.class\.<dIt>apply(item = <l>item\§T#G§\)\ImList#C<§T#G§>\)\ImQueue#C<§T#G§>\
else return <to>ImQueue\ImQueue#C.class\.<tcI>apply(in = <to>ImList\ImList#C.class\.<dIt>apply(item = <l>item\§T#G§\, tail = <ImQueue#C<T#G>>self.<eIU>in\ImList#C<§T#G§>\)\ImList#C<§T#G§>\, out = <ImQueue#C<T#G>>self.<eIU>out\ImList#C<§T#G§>\)\ImQueue#C<§T#G§>\;
    }
    public ImQueue<T> enqueueItem(T item) {
        ERROR: Unknown if(<ImQueue#C<T#G>>self.<dI>isEmpty\bool\) return <to>ImQueue\ImQueue#C.class\.<tcI>apply(in = <to>ImList\ImList#C.class\.<dIt>apply\ImList#C<§T#G§>\, out = <to>ImList\ImList#C.class\.<dIt>apply(item = <l>item\§T#G§\)\ImList#C<§T#G§>\)\ImQueue#C<§T#G§>\
else return <to>ImQueue\ImQueue#C.class\.<tcI>apply(in = <to>ImList\ImList#C.class\.<dIt>apply(item = <l>item\§T#G§\, tail = <ImQueue#C<T#G>>self.<eIU>in\ImList#C<§T#G§>\)\ImList#C<§T#G§>\, out = <ImQueue#C<T#G>>self.<eIU>out\ImList#C<§T#G§>\)\ImQueue#C<§T#G§>\;
    }
    public Tuple2<T, ImQueue<T>> dequeue() {
        ERROR: Unknown if(!(<ImQueue#C<T#G>>self.<eIU>out\ImList#C<§T#G§>\.<rdIo>isEmpty\bool\)) return (<ImQueue#C<T#G>>self.<eIU>out\ImList#C<§T#G§>\.<rdIo>head\(§T#G§)?\, <to>ImQueue\ImQueue#C.class\.<tcI>apply(in = <ImQueue#C<T#G>>self.<eIU>in\ImList#C<§T#G§>\, out = <ImQueue#C<T#G>>self.<eIU>out\ImList#C<§T#G§>\.<dIoa>tail\ImList#C<§T#G§>\)\ImQueue#C<§T#G§>\).cast<(^(§T#G§)?, ^ImQueue#C<T#G>)>
else if(<ImQueue#C<T#G>>self.<eIU>in\ImList#C<§T#G§>\.<rdIo>isEmpty\bool\) return (none<T#G>, <ImQueue#C<T#G>>self).cast<(^(§T#G§)?, ^ImQueue#C<T#G>)>
else {
    local rev : ImList#C<§T#G§> = <ImQueue#C<T#G>>self.<eIU>in\ImList#C<§T#G§>\.<dIa>reverse\ImList#C<§T#G§>\
    return (<l>rev\ImList#C<§T#G§>\.<rdIo>head\(§T#G§)?\, <to>ImQueue\ImQueue#C.class\.<tcI>apply(in = <to>ImList\ImList#C.class\.<dIt>apply\ImList#C<§T#G§>\, out = <l>rev\ImList#C<§T#G§>\.<dIoa>tail\ImList#C<§T#G§>\)\ImQueue#C<§T#G§>\).cast<(^(§T#G§)?, ^ImQueue#C<T#G>)>
};
    }
    public ImQueue(ImList<T> in,ImList<T> out) {
    }
    static final ClassType<ImQueue<T>> type;
}