package objd.actor;

import objd.lang.*;
import objd.concurrent.DefaultPromise;

public final class ActorFuture<T> extends DefaultPromise<T> implements ActorMessage {
    public final Actor receiver;
    @Override
    public Actor receiver() {
        return receiver;
    }
    public final boolean prompt;
    @Override
    public boolean prompt() {
        return prompt;
    }
    public final F0<T> f;
    private volatile boolean _completed;
    private volatile boolean _locked;
    @Override
    public boolean process() {
        if(this._completed) {
            return true;
        } else {
            if(this._locked) {
                return false;
            } else {
                return successValue(this.f.apply());
            }
        }
    }
    public void lock() {
        this._locked = true;
    }
    public void unlock() {
        if(this._locked) {
            this._locked = false;
            this.receiver.mailbox.unlock();
        }
    }
    public boolean isLocked() {
        return this._locked;
    }
    @Override
    public boolean completeValue(final Try<T> value) {
        final boolean ret = ERROR: Unknown <DefaultPromise#C<§T#G§>>super.completeValue(value);
        if(ret) {
            this._completed = true;
            this._locked = false;
        }
        return ret;
    }
    public ActorFuture(final Actor receiver, final boolean prompt, final F0<T> f) {
        this.receiver = receiver;
        this.prompt = prompt;
        this.f = f;
        this._completed = false;
        this._locked = false;
    }
    public String toString() {
        return String.format("ActorFuture(%s, %d)", this.receiver, this.prompt);
    }
}