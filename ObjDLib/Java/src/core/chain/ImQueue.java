package core.chain;

public class ImQueue<T> extends Queue_impl<T> {
    private static final ImQueue<Object> empty = new ImQueue<Object>(ImList().apply<Object>(), ImList().apply<Object>());
    public final ImList<T> in;
    public final ImList<T> out;
    public static ImQueue<T> apply() {
        return this.empty;
    }
    public Iterator<T> iterator() {
        return new QueueIterator<T>(this.in, this.out);
    }
    public boolean isEmpty() {
        return this.in.isEmpty() && this.out.isEmpty();
    }
    public int count() {
        return this.in.count() + this.out.count();
    }
    public ImQueue<T> addItem(T item) {
        if(this.isEmpty()) {
            return new ImQueue<T>(ImList().apply<T>(), ImList().applyItem<T>(item));
        } else {
            return new ImQueue<T>(ImList().applyItemTail<T>(item, this.in), this.out);
        }
    }
    public ImQueue<T> enqueueItem(T item) {
        if(this.isEmpty()) {
            return new ImQueue<T>(ImList().apply<T>(), ImList().applyItem<T>(item));
        } else {
            return new ImQueue<T>(ImList().applyItemTail<T>(item, this.in), this.out);
        }
    }
    public Tuple2<T, ImQueue<T>> dequeue() {
        if(ERROR: Unknown !(<ImQueue#C<T#G>>self.<eIU>out\ImList#C<§T#G§>\.<rdIo>isEmpty\bool\)) {
            return ERROR: Unknown (<ImQueue#C<T#G>>self.<eIU>out\ImList#C<§T#G§>\.<rdIo>head\(§T#G§)?\, <to>ImQueue\ImQueue#C.class\.<tcI>apply(in = <ImQueue#C<T#G>>self.<eIU>in\ImList#C<§T#G§>\, out = <ImQueue#C<T#G>>self.<eIU>out\ImList#C<§T#G§>\.<dIoa>tail\ImList#C<§T#G§>\)\ImQueue#C<§T#G§>\).cast<(^(§T#G§)?, ^ImQueue#C<T#G>)>;
        } else {
            if(this.in.isEmpty()) {
                return ERROR: Unknown (none<T#G>, <ImQueue#C<T#G>>self).cast<(^(§T#G§)?, ^ImQueue#C<T#G>)>;
            } else {
                ImList<T> rev = this.in.reverse();
                return ERROR: Unknown (<l>rev\ImList#C<§T#G§>\.<rdIo>head\(§T#G§)?\, <to>ImQueue\ImQueue#C.class\.<tcI>apply(in = <to>ImList\ImList#C.class\.<dIt>apply\ImList#C<§T#G§>\, out = <l>rev\ImList#C<§T#G§>\.<dIoa>tail\ImList#C<§T#G§>\)\ImQueue#C<§T#G§>\).cast<(^(§T#G§)?, ^ImQueue#C<T#G>)>;
            }
        }
    }
    public ImQueue(ImList<T> in,ImList<T> out) {
        this.in = in;
        this.out = out;
    }
}