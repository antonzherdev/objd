package objd.react;

import objd.lang.*;

public final class Var<T> extends BaseVar<T> {
    public static <T> Var<T> limitedInitialLimits(final T initial, final F<T, T> limits) {
        return new Var(new LimitedVar<T>(initial, limits));
    }
    public static <T> FeedbackVar<T> feedbackInitialFeedback(final T initial, final P<T> feedback) {
        return new FeedbackVar<T>(initial, feedback);
    }
    public Var(final T initial) {
        super(initial);
    }
    public String toString() {
        return "Var";
    }
}