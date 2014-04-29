package objd.concurrent;

import java.util.concurrent.locks.ReentrantLock;

public class Lock {
    private ReentrantLock lock = new ReentrantLock();
    public LockCondition newCondition() {
        return new LockCondition(lock, lock.newCondition());
    }
}
