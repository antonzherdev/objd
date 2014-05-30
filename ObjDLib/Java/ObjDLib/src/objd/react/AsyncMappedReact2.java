package objd.react;

import objd.lang.*;
import objd.concurrent.DispatchQueue;

public final class AsyncMappedReact2<A, B, R> extends ReactExpression<R> {
    public final DispatchQueue queue;
    public final React<A> a;
    public final React<B> b;
    public final F2<A, B, R> f;
    private final Observer<A> obsA;
    private final Observer<B> obsB;
    @Override
    public R calc() {
        return this.f.apply(this.a.value(), this.b.value());
    }
    public AsyncMappedReact2(final DispatchQueue queue, final React<A> a, final React<B> b, final F2<A, B, R> f) {
        super(f.apply(a.value(), b.value()));
        this.queue = queue;
        this.a = a;
        this.b = b;
        this.f = f;
        this.obsA = a.observeF(new P<A>() {
            @Override
            public void apply(final A _) {
                queue.asyncF(new P0() {
                    @Override
                    public void apply() {
                        AsyncMappedReact2.this.recalc();
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
                        AsyncMappedReact2.this.recalc();
                    }
                });
            }
        });
    }
    public String toString() {
        return String.format("AsyncMappedReact2(%s, %s, %s)", this.queue, this.a, this.b);
    }
}