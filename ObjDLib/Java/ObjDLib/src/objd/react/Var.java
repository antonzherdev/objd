package objd.react;

import objd.lang.*;

public abstract class Var<T> extends MReact<T> {
    public static <T> Var<T> applyInitial(final T initial) {
        return new SimpleVar<T>(initial);
    }
    public static <T> Var<T> limitedInitialLimits(final T initial, final F<T, T> limits) {
        return new LimitedVar<T>(initial, limits);
    }
    public static <T> Var<T> feedbackInitialFeedback(final T initial, final P<T> feedback) {
        return new FeedbackVar<T>(initial, feedback);
    }
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
    public Var(final T initial) {
        super(initial);
    }
    public String toString() {
        return "Var";
    }
}