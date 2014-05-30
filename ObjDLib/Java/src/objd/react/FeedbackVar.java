package objd.react;

import objd.lang.*;

public final class FeedbackVar<T> extends Var<T> {
    public final P<T> feedback;
    @Override
    public void setValue(final T value) {
        _setValue(value);
        this.feedback.apply(value);
    }
    public void feedValue(final T value) {
        _setValue(value);
    }
    @Override
    public void updateF(final F<T, T> f) {
        while(true) {
            final T v = this._value.get();
            final T value = f.apply(v);
            if(v.equals(value)) {
                return ;
            }
            if(this._value.compareAndSet(v, value)) {
                notifyValue(value);
                this.feedback.apply(value);
                return ;
            }
        }
    }
    public FeedbackVar(final T initial, final P<T> feedback) {
        super(initial);
        this.feedback = feedback;
    }
    public String toString() {
        return String.format(")");
    }
}