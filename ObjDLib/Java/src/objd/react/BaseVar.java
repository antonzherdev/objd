package objd.react;

import objd.lang.*;

public abstract class BaseVar<T> extends MReact<T> {
    public void setValue(final T value) {
        _setValue(value);
    }
    public void updateF(final F<T, T> f) {
        while(true) {
            final T v = this._value.get();
            final T value = f.apply(v);
            if(v.equals(value)) {
                return ;
            }
            if(this._value.compareAndSet(v, value)) {
                notifyValue(value);
                return ;
            }
        }
    }
    public BaseVar(final T initial) {
        super(initial);
    }
    public String toString() {
        return "BaseVar";
    }
}