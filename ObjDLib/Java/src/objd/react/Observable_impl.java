package objd.react;

import objd.lang.*;

public abstract class Observable_impl<T> implements Observable<T> {
    public Observable_impl() {
    }
    public Observer<T> observeF(final P<T> f) {
        final Observer<T> obs = new Observer<T>(this, f);
        attachObserver(obs);
        return obs;
    }
}