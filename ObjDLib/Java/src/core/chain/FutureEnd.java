package core.chain;

public class FutureEnd<T> {
    private final Promise<Seq<T>> _promise = ERROR: Unknown <to>Promise\Promise#C.class\.<dIt>apply\Promise#C<§^Seq#T<T#G>§>\;
    private boolean _stopped = ERROR: Unknown False;
    private AtomicInt _counter = new AtomicInt();
    private boolean _ended = ERROR: Unknown False;
    private AtomicBool _yielded = new AtomicBool();
    private MArray<T> _array;
    public Future<Seq<T>> future() {
        return ERROR: Unknown <FutureEnd#C<T#G>>self.<ep>_promise\Promise#C<§^Seq#T<T#G>§>\;
    }
    public Yield<Future<T>> yield() {
        ERROR: Unknown local var _i : int = 0;
        return ERROR: Unknown <to>Yield\Yield#C.class\.<dIt>apply(begin = size : uint -> int = {
    (<FutureEnd#C<T#G>>self.<emp>_array\(^MArray#C<§T#G§>)?\ = some(<to>MArray\MArray#C.class\.<dIt>apply(capacity = <l>size\uint\)\MArray#C<§T#G§>\)\(^MArray#C<§T#G§>)?\)
    return 0
}, yield = fut : §^Future#C<T#G>§ -> int = {
    if(!(<FutureEnd#C<T#G>>self.<emp>_stopped\bool\)) {
    <FutureEnd#C<T#G>>self.<emp>_counter\AtomicInt#C\.<dI>incrementAndGet\int4\
    <FutureEnd#C<T#G>>self.<emp>_array\(^MArray#C<§T#G§>)?\.get.<rdIa>append(item = nil)\void\
    local i : int = <lm>_i\int\
    <lm>_i\int\++
    <l>fut\§^Future#C<T#G>§\.<dIa>onComplete(f = tr : Try#C<§T#G§> -> void = if(!(<FutureEnd#C<T#G>>self.<emp>_stopped\bool\)) {
    if(<l>tr\Try#C<§T#G§>\.<dI>isFailure\bool\) {
    (<FutureEnd#C<T#G>>self.<emp>_stopped\bool\ = True)
    <FutureEnd#C<T#G>>self.<ep>_promise\Promise#C<§^Seq#T<T#G>§>\.<dIa>failure(reason = <l>tr\Try#C<§T#G§>\)\bool\
}
else if(!(<FutureEnd#C<T#G>>self.<emp>_stopped\bool\)) {
    <FutureEnd#C<T#G>>self.<emp>_array\(^MArray#C<§T#G§>)?\.get.<rdI>set(index = <l>i\int\.cast<uint>, item = <l>tr\Try#C<§T#G§>\.<dIa>get\§T#G§\)\void\
    <to>Memory\Memory#C.class\.<dIt>memoryBarrier\void\
    local r : int4 = <FutureEnd#C<T#G>>self.<emp>_counter\AtomicInt#C\.<dI>decrementAndGet\int4\
    <to>Memory\Memory#C.class\.<dIt>memoryBarrier\void\
    if((<FutureEnd#C<T#G>>self.<emp>_ended\bool\ && (<l>r\int4\ == 0))) {
    <to>Memory\Memory#C.class\.<dIt>memoryBarrier\void\
    if(!(<FutureEnd#C<T#G>>self.<emp>_yielded\AtomicBool#C\.<dI>getAndSet(newValue = True)\bool\)) {
    <FutureEnd#C<T#G>>self.<ep>_promise\Promise#C<§^Seq#T<T#G>§>\.<dIa>success(value = <FutureEnd#C<T#G>>self.<emp>_array\(^MArray#C<§T#G§>)?\.get)\bool\
}
}
}
})\void\
}
    if(<FutureEnd#C<T#G>>self.<emp>_stopped\bool\) return 1
else return 0
}, end = res : int -> int = {
    (<FutureEnd#C<T#G>>self.<emp>_ended\bool\ = True)
    <to>Memory\Memory#C.class\.<dIt>memoryBarrier\void\
    if((<FutureEnd#C<T#G>>self.<emp>_counter\AtomicInt#C\.<dI>intValue\int4\ == 0)) {
    <to>Memory\Memory#C.class\.<dIt>memoryBarrier\void\
    if(!(<FutureEnd#C<T#G>>self.<emp>_yielded\AtomicBool#C\.<dI>getAndSet(newValue = True)\bool\)) {
    <FutureEnd#C<T#G>>self.<ep>_promise\Promise#C<§^Seq#T<T#G>§>\.<dIa>success(value = <FutureEnd#C<T#G>>self.<emp>_array\(^MArray#C<§T#G§>)?\.get)\bool\
}
}
    return <l>res\int\
})\Yield#C<§^Future#C<T#G>§>\;
    }
    public FutureEnd() {
    }
    static final ClassType<FutureEnd<T>> type;
}