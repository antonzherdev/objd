package core.chain;

public class DefaultPromise<T> extends Promise<T> {
    private final AtomicObject<Object> _state = AtomicObject().applyValue<Object>(ERROR: Unknown []);
    @Override
    public Try<T> result() {
        ERROR: Unknown local v : any = <DefaultPromise#C<T#G>>self.<ep>_state\AtomicObject#C<§^any§>\.<dI>value\§^any§\;
        if(v.ERROR: Unknown is<Try#C<T#G>>) {
            return v.ERROR: Unknown cast<Try#C<T#G>>;
        } else {
            return null;
        }
    }
    @Override
    public boolean completeValue(Try<T> value) {
        ERROR: Unknown while(True) {
    local v : any = <DefaultPromise#C<T#G>>self.<ep>_state\AtomicObject#C<§^any§>\.<dI>value\§^any§\
    if(<l>v\any\.is<Try#C<T#G>>) return False
else {
    if(<DefaultPromise#C<T#G>>self.<ep>_state\AtomicObject#C<§^any§>\.<dI>compareAndSet(oldValue = <l>v\any\, newValue = <l>value\Try#C<§T#G§>\)\bool\) {
    <l>v\any\.cast<[^Try#C<T#G> -> void]>.<rdIo>for(each = f : §^Try#C<T#G> -> void§ -> void = <l>f\§^Try#C<T#G> -> void§\.<d>apply( = <l>value\Try#C<§T#G§>\)\void\)\void\
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
    local v : any = <DefaultPromise#C<T#G>>self.<ep>_state\AtomicObject#C<§^any§>\.<dI>value\§^any§\
    if(<l>v\any\.is<Try#C<T#G>>) {
    <l>f\Try#C<§T#G§> -> void\.<d>apply( = <l>v\any\.cast<Try#C<T#G>>)\void\
    return nil
}
else {
    local vv : [^Try#C<T#G> -> void] = <l>v\any\.cast<[^Try#C<T#G> -> void]>
    if(<DefaultPromise#C<T#G>>self.<ep>_state\AtomicObject#C<§^any§>\.<dI>compareAndSet(oldValue = <l>vv\[^Try#C<§T#G§> -> void]\, newValue = <l>vv\[^Try#C<§T#G§> -> void]\.<dIo>add(item = <l>f\Try#C<§T#G§> -> void\)\[^Try#C<§T#G§> -> void]\)\bool\) {
    return nil
}
}
};
    }
    public DefaultPromise() {
    }
}