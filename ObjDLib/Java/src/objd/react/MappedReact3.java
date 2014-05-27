package objd.react;

import objd.lang.*;

public final class MappedReact3<A, B, C, R> extends ReactExpression<R> {
    public final React<A> a;
    public final React<B> b;
    public final React<C> c;
    public final F3<A, B, C, R> f;
    private final Observer<A> obsA;
    private final Observer<B> obsB;
    private final Observer<C> obsC;
    @Override
    public R calc() {
        return this.f.apply(this.a.value(), this.b.value(), this.c.value());
    }
    public MappedReact3(final React<A> a, final React<B> b, final React<C> c, final F3<A, B, C, R> f) {
        super(f.apply(a.value(), b.value(), c.value()));
        this.a = a;
        this.b = b;
        this.c = c;
        this.f = f;
        this.obsA = a.observeF(new P<A>() {
            @Override
            public void apply(final A newValue) {
                _setValue(f.apply(newValue, b.value(), c.value()));
            }
        });
        this.obsB = b.observeF(new P<B>() {
            @Override
            public void apply(final B newValue) {
                _setValue(f.apply(a.value(), newValue, c.value()));
            }
        });
        this.obsC = c.observeF(new P<C>() {
            @Override
            public void apply(final C newValue) {
                _setValue(f.apply(a.value(), b.value(), newValue));
            }
        });
    }
    public String toString() {
        return String.format("MappedReact3(%s, %s, %s)", this.a, this.b, this.c);
    }
}