package core.chain;

public class MQueue<T> implements Queue<T> {
    private ImQueue<T> _queue = ImQueue().apply<T>();
    public void enqueueItem(T item) {
        ERROR: Unknown (<MQueue#C<T#G>>self.<emp>_queue\ImQueue#C<§T#G§>\ = <MQueue#C<T#G>>self.<emp>_queue\ImQueue#C<§T#G§>\.<dI>add(item = <l>item\§T#G§\)\ImQueue#C<§T#G§>\);
    }
    public T dequeue() {
        ERROR: Unknown local p : (^(§T#G§)?, ^ImQueue#C<§T#G§>) = <MQueue#C<T#G>>self.<emp>_queue\ImQueue#C<§T#G§>\.<dI>dequeue\(^(§T#G§)?, ^ImQueue#C<§T#G§>)\;
        ERROR: Unknown (<MQueue#C<T#G>>self.<emp>_queue\ImQueue#C<§T#G§>\ = <l>p\(^(§T#G§)?, ^ImQueue#C<§T#G§>)\.<eIU>b\^ImQueue#C<§T#G§>\);
        return p.a;
    }
    public int count() {
        return _queue.count();
    }
    public MQueue() {
    }
}