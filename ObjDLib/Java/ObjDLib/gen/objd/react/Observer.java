package objd.react;

import objd.lang.*;

public class Observer<T> {
    public final Observable<T> observable;
    public final P<T> f;
    public void detach() {
        this.observable.detachObserver(this);
    }
    @Override
    public void finalize() throws Throwable {
        super.finalize();
        this.observable.detachObserver(null);
    }
    public Observer(final Observable<T> observable, final P<T> f) {
        this.observable = observable;
        this.f = f;
    }
    public String toString() {
        return String.format("Observer(%s)", this.observable);
    }
}