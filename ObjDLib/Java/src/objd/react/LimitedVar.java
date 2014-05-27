package objd.react;

import objd.lang.*;

public final class LimitedVar<T> extends BaseVar<T> {
    public final F<T, T> limits;
    @Override
    public void setValue(final T value) {
        _setValue(this.limits.apply(value));
    }
    @Override
    public void updateF(final F<T, T> f) {
        while(true) {
            final T v = this._value.get();
            final T value = this.limits.apply(f.apply(v));
            if(v.equals(value)) {
                return ;
            }
            if(this._value.compareAndSet(v, value)) {
                notifyValue(value);
                return ;
            }
        }
    }
    public LimitedVar(final T initial, final F<T, T> limits) {
        super(limits.apply(initial));
        this.limits = limits;
    }
    public String toString() {
        return String.format(")");
    }
}