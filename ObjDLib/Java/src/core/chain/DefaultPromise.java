package core.chain;

public class DefaultPromise<T> extends Promise<T> {
    private final AtomicObject<Object> _state = new AtomicObject<Object>(ImArray.fromObjects());
    @Override
    public Try<T> result() {
        Object v = this._state.get();
        if(v instanceof Try) {
            return ((Try<T>)v);
        } else {
            return null;
        }
    }
    @Override
    public boolean completeValue(Try<T> value) {
        while(true) {
            Object v = this._state.get();
            if(v instanceof Try) {
                return false;
            } else {
                if(this._state.compareAndSet(v, value)) {
                    {
                        Iterator<P<Try<T>>> __inline__0_1_0_0_i = ((ImArray<P<Try<T>>>)v).iterator();
                        while(__inline__0_1_0_0_i.hasNext()) {
                            P<Try<T>> f = __inline__0_1_0_0_i.next();
                            f.apply(value);
                        }
                    }
                    return true;
                }
            }
        }
    }
    @Override
    public boolean successValue(T value) {
        return completeValue(new Success<T>(value));
    }
    @Override
    public boolean failureReason(Object reason) {
        return completeValue(new Failure<T>(this.result()));
    }
    @Override
    public void onCompleteF(P<Try<T>> f) {
        while(true) {
            Object v = this._state.get();
            if(v instanceof Try) {
                f.apply(((Try<T>)v));
                return ;
            } else {
                ImArray<P<Try<T>>> vv = ((ImArray<P<Try<T>>>)v);
                if(this._state.compareAndSet(vv, vv.addItem(f))) {
                    return ;
                }
            }
        }
    }
    public DefaultPromise() {
    }
}