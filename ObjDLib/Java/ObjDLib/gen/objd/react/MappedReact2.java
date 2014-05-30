package objd.react;

import objd.lang.*;

public final class MappedReact2<A, B, R> extends ReactExpression<R> {
    public final React<A> a;
    public final React<B> b;
    public final F2<A, B, R> f;
    private final Observer<A> obsA;
    private final Observer<B> obsB;
    @Override
    public R calc() {
        return this.f.apply(this.a.value(), this.b.value());
    }
    public MappedReact2(final React<A> a, final React<B> b, final F2<A, B, R> f) {
        super(f.apply(a.value(), b.value()));
        this.a = a;
        this.b = b;
        this.f = f;
        this.obsA = a.observeF(new P<A>() {
            @Override
            public void apply(final A newValue) {
                _setValue(f.apply(newValue, b.value()));
            }
        });
        this.obsB = b.observeF(new P<B>() {
            @Override
            public void apply(final B newValue) {
                _setValue(f.apply(a.value(), newValue));
            }
        });
    }
    public String toString() {
        return String.format("MappedReact2(%s, %s)", this.a, this.b);
    }
}