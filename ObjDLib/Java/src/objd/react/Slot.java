package objd.react;

import objd.lang.*;

public final class Slot<T> extends MReact<T> {
    private React<T> _base;
    private Observer<T> _observer;
    public void connectTo(final React<T> to) {
        synchronized(this) {
            this._base = to;
            if(this._observer != null) {
                this._observer.detach();
            }
            this._observer = to.observeF(new P<T>() {
                @Override
                public void apply(final T newValue) {
                    _setValue(newValue);
                }
            });
            _setValue(to.value());
        }
    }
    public void setValue(final T value) {
        connectTo(new Val<T>(value));
    }
    public Slot(final T initial) {
        super(initial);
    }
    public String toString() {
        return "Slot";
    }
}