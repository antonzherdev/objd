package core.chain;

public class DefaultPromise<T> extends Promise<T> {
    private final AtomicObject<Object> _state = new AtomicObject<Object>(ImArray.fromObjects());
    @Override
    public Try<T> result() {
        Object v = this._state.get();
        if(v.ERROR: Unknown is<Try#C<T#G>>) {
            return v.ERROR: Unknown cast<Try#C<T#G>>;
        } else {
            return null;
        }
    }
    @Override
    public boolean completeValue(Try<T> value) {
        while(true) {
            Object v = this._state.get();
            if(v.ERROR: Unknown is<Try#C<T#G>>) {
                return false;
            } else {
                if(this._state.compareAndSetOldValueNewValue(v, value)) {
                    {
                        Iterator<P<Try<T>>> __inline__0_1_0_0_i = v.ERROR: Unknown cast<[^Try#C<T#G> -> void]>.iterator();
                        while(__inline__0_1_0_0_i.hasNext()) {
                            P<Try<T>> f = __inline__0_1_0_0_i.next();
                            f.apply(value);
                        }
                    }
                    return true;
                }
            }
        }
        return false;
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
            if(v.ERROR: Unknown is<Try#C<T#G>>) {
                f.apply(v.ERROR: Unknown cast<Try#C<T#G>>);
                return null;
            } else {
                ImArray<P<Try<T>>> vv = v.ERROR: Unknown cast<[^Try#C<T#G> -> void]>;
                if(this._state.compareAndSetOldValueNewValue(vv, vv.addItem(f))) {
                    return null;
                }
            }
        }
    }
    public DefaultPromise() {
    }
}