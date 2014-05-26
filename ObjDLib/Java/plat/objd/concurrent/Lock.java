package objd.concurrent;

import java.util.concurrent.locks.ReentrantLock;

public class Lock {
    private ReentrantLock lock = new ReentrantLock();

    public void lock() {
        lock.lock();
    }

    public void unlock() {
        lock.unlock();
    }
}
