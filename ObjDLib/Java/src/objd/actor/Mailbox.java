package objd.actor;

import objd.lang.*;
import objd.concurrent.AtomicBool;
import objd.concurrent.ConcurrentQueue;
import objd.concurrent.DispatchQueue;

public class Mailbox {
    private volatile boolean _stopped;
    private final AtomicBool _scheduled;
    private final ConcurrentQueue<ActorMessage> _queue;
    private volatile boolean _locked;
    public void sendMessage(final ActorMessage message) {
        if(this._stopped) {
            return ;
        }
        if(message.prompt()) {
            if(!(this._scheduled.getAndSet(true))) {
                if(this._queue.isEmpty()) {
                    message.process();
                    if(this._queue.isEmpty()) {
                        this._scheduled.set(false);
                        if(!(this._queue.isEmpty())) {
                            this.trySchedule();
                        }
                    } else {
                        this.schedule();
                    }
                } else {
                    this._queue.enqueueItem(message);
                    this.schedule();
                }
            } else {
                this._queue.enqueueItem(message);
                this.trySchedule();
            }
        } else {
            this._queue.enqueueItem(message);
            this.trySchedule();
        }
    }
    private void trySchedule() {
        if(!(this._scheduled.getAndSet(true))) {
            this.schedule();
        }
    }
    private void schedule() {
        if(!(this._stopped)) {
            DispatchQueue.aDefault.asyncF(new P0() {
                @Override
                public void apply() {
                    Memory.autoreleasePoolStart();
                    Mailbox.this.processQueue();
                    Memory.autoreleasePoolEnd();
                }
            });
        }
    }
    private void processQueue() {
        int left = 5;
        this._locked = false;
        while(left > 0) {
            final ActorMessage msg = this._queue.dequeueWhen(new F<ActorMessage, Boolean>() {
                @Override
                public Boolean apply(final ActorMessage message) {
                    if(message.process()) {
                        return true;
                    } else {
                        Mailbox.this._locked = true;
                        return false;
                    }
                }
            });
            if(msg == null) {
                break;
            }
            left--;
        }
        if(this._locked) {
        } else {
            if(this._queue.isEmpty()) {
                this._scheduled.set(false);
                if(!(this._queue.isEmpty())) {
                    this.trySchedule();
                }
            } else {
                this.schedule();
            }
        }
    }
    public void unlock() {
        if(this._locked) {
            this._locked = false;
            this.schedule();
        }
    }
    public void stop() {
        this._stopped = true;
        this._queue.clear();
    }
    public boolean isEmpty() {
        return this._queue.isEmpty();
    }
    public Mailbox() {
        this._stopped = false;
        this._scheduled = new AtomicBool();
        this._queue = new ConcurrentQueue<ActorMessage>();
        this._locked = false;
    }
    public String toString() {
        return "Mailbox";
    }
}