package objd.concurrent;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

public class ConditionLock {
    private int state;
    private ReentrantLock lock = new ReentrantLock();
    private Condition condition = lock.newCondition();
    public ConditionLock(int i) {
        state = i;
    }

    public void lock() {
        lock.lock();
    }

    public void unlockWithCondition(int i) {
        state = i;
        condition.signalAll();
        lock.unlock();
    }

    public boolean lockWhenConditionPeriod(int i, double period) {
        lock.lock();
        try {
             while(state != i) if(!condition.await((long) (period * 1000000000), TimeUnit.NANOSECONDS)) return false;
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
        return true;
    }

    public void unlock() {
        lock.unlock();
    }

    public void lockWhenCondition(int i) {
        lock.lock();
        try {
            while(state != i) condition.await();
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
    }
}
