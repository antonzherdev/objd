package objd.react;

import objd.lang.*;
import objd.concurrent.DispatchQueue;

public final class AsyncMappedReact<A, R> extends ReactExpression<R> {
    public final DispatchQueue queue;
    public final React<A> a;
    public final F<A, R> f;
    private final Observer<A> obsA;
    @Override
    public R calc() {
        return this.f.apply(this.a.value());
    }
    public AsyncMappedReact(final DispatchQueue queue, final React<A> a, final F<A, R> f) {
        super(f.apply(a.value()));
        this.queue = queue;
        this.a = a;
        this.f = f;
        this.obsA = a.observeF(new P<A>() {
            @Override
            public void apply(final A _) {
                queue.asyncF(new P0() {
                    @Override
                    public void apply() {
                        AsyncMappedReact.this.recalc();
                    }
                });
            }
        });
    }
    public String toString() {
        return String.format("AsyncMappedReact(%s, %s)", this.queue, this.a);
    }
}