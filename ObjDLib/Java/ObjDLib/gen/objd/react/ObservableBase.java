package objd.react;

import objd.lang.*;
import objd.concurrent.AtomicObject;
import objd.collection.ImArray;

public interface ObservableBase<T> extends Observable<T> {
    @Override
    void attachObserver(final Observer<T> observer);
    @Override
    void detachObserver(final Observer<T> observer);
    String toString();
}