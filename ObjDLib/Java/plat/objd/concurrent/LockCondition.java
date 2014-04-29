package objd.concurrent;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

public class LockCondition {
    private final ReentrantLock lock;
    private final Condition condition;

    public LockCondition(ReentrantLock lock, Condition condition) {
        this.lock = lock;
        this.condition = condition;
    }

    public void signal() {
        condition.signalAll();
    }

    public void unlockedSignal() {
        lock.lock();
        condition.signalAll();
        lock.unlock();
    }

    public void awaitPeriod(float period) {
        try {
            condition.await((long) (period*1000000000), TimeUnit.NANOSECONDS);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
    }
    public void unlockedAwaitPeriod(float period) {
        lock.lock();
        try {
            condition.await((long) (period*1000000000), TimeUnit.NANOSECONDS);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
        lock.unlock();
    }

    public void await() {
        try {
            condition.await();
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
    }

    public void unlockedAwait() {
        lock.lock();
        try {
            condition.await();
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
        lock.unlock();
    }
}
