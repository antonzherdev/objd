package core.chain;

public class DefaultPromise<T> extends Promise<T> {
    private final AtomicObject<Object> _state = new AtomicObject<Object>(ERROR: Unknown []);
    @Override
    public Try<T> result() {
        ERROR: Unknown local v : T#G = return <DefaultPromise#C<T#G>>self.<ep>_state\AtomicObject#C<§^any§>\.<rdIb>get\§T#G§\;
        if(v.ERROR: Unknown is<Try#C<T#G>>) {
            return v.ERROR: Unknown cast<Try#C<T#G>>;
        } else {
            return null;
        }
    }
    @Override
    public boolean completeValue(Try<T> value) {
        ERROR: Unknown while(True) {
    local v : T#G = return <DefaultPromise#C<T#G>>self.<ep>_state\AtomicObject#C<§^any§>\.<rdIb>get\§T#G§\
    if(<l>v\§T#G§\.is<Try#C<T#G>>) return False
else {
    if(<DefaultPromise#C<T#G>>self.<ep>_state\AtomicObject#C<§^any§>\.<rdIb>compareAndSet(oldValue = <l>v\§T#G§\, newValue = <l>value\Try#C<§T#G§>\)\bool\) {
    <l>v\§T#G§\.cast<[^Try#C<T#G> -> void]>.<rdIo>for(each = f : §^Try#C<T#G> -> void§ -> void = <l>f\§^Try#C<T#G> -> void§\.<d>apply( = <l>value\Try#C<§T#G§>\)\void\)\void\
    return True
}
}
};
        return ERROR: Unknown False;
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
        ERROR: Unknown while(True) {
    local v : T#G = return <DefaultPromise#C<T#G>>self.<ep>_state\AtomicObject#C<§^any§>\.<rdIb>get\§T#G§\
    if(<l>v\§T#G§\.is<Try#C<T#G>>) {
    <l>f\Try#C<§T#G§> -> void\.<d>apply( = <l>v\§T#G§\.cast<Try#C<T#G>>)\void\
    return nil
}
else {
    local vv : [^Try#C<T#G> -> void] = <l>v\§T#G§\.cast<[^Try#C<T#G> -> void]>
    if(<DefaultPromise#C<T#G>>self.<ep>_state\AtomicObject#C<§^any§>\.<rdIb>compareAndSet(oldValue = <l>vv\[^Try#C<§T#G§> -> void]\, newValue = <l>vv\[^Try#C<§T#G§> -> void]\.<dIob>add(item = <l>f\Try#C<§T#G§> -> void\)\[^Try#C<§T#G§> -> void]\)\bool\) {
    return nil
}
}
};
    }
    public DefaultPromise() {
    }
}