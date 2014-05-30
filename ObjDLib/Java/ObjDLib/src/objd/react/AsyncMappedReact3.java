package objd.react;

import objd.lang.*;
import objd.concurrent.DispatchQueue;

public final class AsyncMappedReact3<A, B, C, R> extends ReactExpression<R> {
    public final DispatchQueue queue;
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
    public AsyncMappedReact3(final DispatchQueue queue, final React<A> a, final React<B> b, final React<C> c, final F3<A, B, C, R> f) {
        super(f.apply(a.value(), b.value(), c.value()));
        this.queue = queue;
        this.a = a;
        this.b = b;
        this.c = c;
        this.f = f;
        this.obsA = a.observeF(new P<A>() {
            @Override
            public void apply(final A _) {
                queue.asyncF(new P0() {
                    @Override
                    public void apply() {
                        AsyncMappedReact3.this.recalc();
                    }
                });
            }
        });
        this.obsB = b.observeF(new P<B>() {
            @Override
            public void apply(final B _) {
                queue.asyncF(new P0() {
                    @Override
                    public void apply() {
                        AsyncMappedReact3.this.recalc();
                    }
                });
            }
        });
        this.obsC = c.observeF(new P<C>() {
            @Override
            public void apply(final C _) {
                queue.asyncF(new P0() {
                    @Override
                    public void apply() {
                        AsyncMappedReact3.this.recalc();
                    }
                });
            }
        });
    }
    public String toString() {
        return String.format("AsyncMappedReact3(%s, %s, %s, %s)", this.queue, this.a, this.b, this.c);
    }
}