package objd.react;

import objd.lang.*;

public final class FlatMappedReact<A, R> extends ReactExpression<R> {
    public final React<A> a;
    public final F<A, React<R>> f;
    private final Observer<A> obsA;
    @Override
    public R calc() {
        return this.f.apply(this.a.value()).value();
    }
    public FlatMappedReact(final React<A> a, final F<A, React<R>> f) {
        super(f.apply(a.value()).value());
        this.a = a;
        this.f = f;
        this.obsA = a.observeF(new P<A>() {
            @Override
            public void apply(final A newValue) {
                _setValue(f.apply(newValue).value());
            }
        });
    }
    public String toString() {
        return String.format("FlatMappedReact(%s)", this.a);
    }
}