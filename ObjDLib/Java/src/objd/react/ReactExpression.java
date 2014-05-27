package objd.react;

import objd.lang.*;

public abstract class ReactExpression<T> extends MReact<T> {
    protected abstract T calc();
    protected void recalc() {
        _setValue(this.calc());
    }
    public ReactExpression(final T initial) {
        super(initial);
    }
    public String toString() {
        return "ReactExpression";
    }
}