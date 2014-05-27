package objd.react;

import objd.lang.*;
import objd.collection.ImArray;
import objd.concurrent.AtomicObject;
import objd.collection.Iterator;

public abstract class ObservableBase_impl<T> extends Observable_impl<T> implements ObservableBase<T> {
    public ObservableBase_impl() {
    }
    @Override
    public void attachObserver(final Observer<T> observer) {
        while(true) {
            final ImArray<Weak<Observer<T>>> v = this._observers.get();
            if(this._observers.compareAndSet(v, v.addItem(new Weak<Observer<T>>(observer)))) {
                return ;
            }
        }
    }
    @Override
    public void detachObserver(final Observer<T> observer) {
        final F<Weak<Observer<T>>, Boolean> p = ((observer == null) ? (new F<Weak<Observer<T>>, Boolean>() {
            @Override
            public Boolean apply(final Weak<Observer<T>> l) {
                return !(l.isEmpty());
            }
        }) : (new F<Weak<Observer<T>>, Boolean>() {
            @Override
            public Boolean apply(final Weak<Observer<T>> l) {
                final Observer<T> lv = l.value;
                return lv != observer && lv != null;
            }
        }));
        while(true) {
            final ImArray<Weak<Observer<T>>> v = this._observers.get();
            final ImArray<Weak<Observer<T>>> nv = v.chain().filterWhen(p).toArray();
            if(this._observers.compareAndSet(v, nv)) {
                return ;
            }
        }
    }
    private final AtomicObject<ImArray<Weak<Observer<T>>>> _observers;
    protected void notifyValue(final T value) {
        {
            final Iterator<Weak<Observer<T>>> __il__0i = this._observers.get().iterator();
            while(__il__0i.hasNext()) {
                final Weak<Observer<T>> o = __il__0i.next();
                {
                    final Observer<T> v = o.value;
                    final P<T> __tmp_0rp0_1u = ((v == null) ? (null) : (v.f));
                    ((__tmp_0rp0_1u == null) ? (null) : (__tmp_0rp0_1u.apply(value)));
                }
            }
        }
    }
    protected boolean hasObservers() {
        return !(this._observers.get().isEmpty());
    }
}