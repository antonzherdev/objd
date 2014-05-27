package objd.react;

import objd.lang.*;

public final class MappedReact<A, R> extends ReactExpression<R> {
    public final React<A> a;
    public final F<A, R> f;
    private final Observer<A> obsA;
    @Override
    public R calc() {
        return this.f.apply(this.a.value());
    }
    public MappedReact(final React<A> a, final F<A, R> f) {
        super(f.apply(a.value()));
        this.a = a;
        this.f = f;
        this.obsA = a.observeF(new P<A>() {
            @Override
            public void apply(final A newValue) {
                _setValue(f.apply(newValue));
            }
        });
    }
    public String toString() {
        return String.format("MappedReact(%s)", this.a);
    }
}