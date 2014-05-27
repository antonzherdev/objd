package objd.concurrent;

import objd.lang.*;
import objd.collection.Queue_impl;

public class ConcurrentQueue<T> extends Queue_impl<T> {
    private ConcurrentQueueNode<T> _head;
    private ConcurrentQueueNode<T> _tail;
    private final Lock hLock;
    private final Lock tLock;
    private final AtomicInt _count;
    public int count() {
        return this._count.get();
    }
    public void enqueueItem(final T item) {
        final ConcurrentQueueNode<T> node = ConcurrentQueueNode.<T>applyItem(item);
        this.tLock.lock();
        this._tail.next = node;
        this._tail = node;
        this._count.incrementAndGet();
        this.tLock.unlock();
    }
    public T dequeue() {
        this.hLock.lock();
        final T ret;
        {
            final ConcurrentQueueNode<T> newHead = this._head.next;
            if(newHead != null) {
                final T item = newHead.item;
                newHead.item = null;
                this._head = ((ConcurrentQueueNode<T>)(((ConcurrentQueueNode)(newHead))));
                this._count.decrementAndGet();
                ret = item;
            } else {
                ret = null;
            }
        }
        this.hLock.unlock();
        return ret;
    }
    public T dequeueWhen(final F<T, Boolean> when) {
        this.hLock.lock();
        final T ret;
        {
            final ConcurrentQueueNode<T> newHead = this._head.next;
            if(newHead != null) {
                final T item = newHead.item;
                if(item == null) {
                    throw new NullPointerException();
                }
                if(when.apply(item)) {
                    newHead.item = null;
                    this._head = ((ConcurrentQueueNode<T>)(((ConcurrentQueueNode)(newHead))));
                    this._count.decrementAndGet();
                    ret = item;
                } else {
                    ret = null;
                }
            } else {
                ret = null;
            }
        }
        this.hLock.unlock();
        return ret;
    }
    public void clear() {
        this.hLock.lock();
        this._head = this._tail;
        this._head.item = null;
        this._count.set(((int)(0)));
        this.hLock.unlock();
    }
    public T peek() {
        this.hLock.lock();
        final ConcurrentQueueNode<T> node = this._head;
        final ConcurrentQueueNode<T> newHead = node.next;
        if(newHead == null) {
            this.hLock.unlock();
            return null;
        }
        final T item = ((newHead == null) ? (null) : (newHead.item));
        this.hLock.unlock();
        return item;
    }
    public boolean isEmpty() {
        return this._count.get() == 0;
    }
    public ConcurrentQueue() {
        this._head = new ConcurrentQueueNode<T>();
        this._tail = this._head;
        this.hLock = new Lock();
        this.tLock = new Lock();
        this._count = new AtomicInt();
    }
    public String toString() {
        return "ConcurrentQueue";
    }
}