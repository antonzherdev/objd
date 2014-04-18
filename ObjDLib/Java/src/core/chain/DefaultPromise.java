package core.chain;

public class DefaultPromise<T> extends Promise<T> {
    private final AtomicObject<Object> _state = ERROR: Unknown <to>AtomicObject\AtomicObject#C.class\.<dIt>apply(value = [])\AtomicObject#C<§^any§>\;
    public Try<T> result() {
        ERROR: Unknown local v : any = <DefaultPromise#C<T#G>>self.<ep>_state\AtomicObject#C<§^any§>\.<dI>value\§^any§\;
        ERROR: Unknown if(<l>v\any\.is<Try#C<T#G>>) return some(<l>v\any\.cast<Try#C<T#G>>)\(^Try#C<T#G>)?\
else return none<^Try#C<T#G>>;
    }
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
    public boolean successValue(T value) {
        return ERROR: Unknown <DefaultPromise#C<T#G>>self.<dIo>complete(value = <to>Success\Success#C.class\.<tcI>apply(get = <l>value\§T#G§\)\Success#C<§T#G§>\)\bool\;
    }
    public boolean failureReason(Object reason) {
        return ERROR: Unknown <DefaultPromise#C<T#G>>self.<dIo>complete(value = <to>Failure\Failure#C.class\.<tcI>apply(reason = <DefaultPromise#C<T#G>>self.<dIo>result\(^Try#C<§T#G§>)?\)\Failure#C<§T#G§>\)\bool\;
    }
    public void onCompleteF(F<Try<T>, Void> f) {
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
    static final ClassType<DefaultPromise<T>> type;
}