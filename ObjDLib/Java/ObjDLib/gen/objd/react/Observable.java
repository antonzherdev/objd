package objd.react;

import objd.lang.*;

public interface Observable<T> {
    void attachObserver(final Observer<T> observer);
    void detachObserver(final Observer<T> observer);
    Observer<T> observeF(final P<T> f);
    String toString();
}