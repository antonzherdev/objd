package objd.react;

import objd.lang.*;
import objd.concurrent.AtomicObject;
import objd.collection.ImArray;

public interface ObservableBase<T> extends Observable<T> {
    private final AtomicObject<ImArray<Weak<Observer<T>>>> _observers;
    @Override
    void attachObserver(final Observer<T> observer);
    @Override
    void detachObserver(final Observer<T> observer);
    void notifyValue(final T value);
    boolean hasObservers();
    String toString();
}