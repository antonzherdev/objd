package core.chain;

public class DefaultPromise<T> extends Promise<T> {
    private final AtomicObject<Object> _state;
    @Override
    public Try<T> result() {
        final Object v = this._state.get();
        if(v instanceof Try) {
            return ((Try<T>)v);
        } else {
            return null;
        }
    }
    @Override
    public boolean completeValue(final Try<T> value) {
        while(true) {
            final Object v = this._state.get();
            if(v instanceof Try) {
                return false;
            } else {
                if(this._state.compareAndSet(v, value)) {
                    ((ImArray<P<Try<T>>>)v).forEach(new P<P<Try<T>>>() {
                        @Override
                        public void apply(final P<Try<T>> f) {
                            f.apply(value);
                        }
                    });
                    return true;
                }
            }
        }
    }
    @Override
    public boolean successValue(final T value) {
        return completeValue(new Success<T>(value));
    }
    @Override
    public boolean failureReason(final Object reason) {
        return completeValue(new Failure<T>(this.result()));
    }
    @Override
    public void onCompleteF(final P<Try<T>> f) {
        while(true) {
            final Object v = this._state.get();
            if(v instanceof Try) {
                f.apply(((Try<T>)v));
                return ;
            } else {
                final ImArray<P<Try<T>>> vv = ((ImArray<P<Try<T>>>)v);
                if(this._state.compareAndSet(vv, vv.addItem(f))) {
                    return ;
                }
            }
        }
    }
    public DefaultPromise() {
        this._state = new AtomicObject<Object>(ImArray.fromObjects());
    }
}