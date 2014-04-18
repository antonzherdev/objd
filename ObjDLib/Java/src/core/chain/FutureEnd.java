package core.chain;

public class FutureEnd<T> {
    private Promise<Seq<T>> _promise;
    private boolean _stopped;
    private AtomicInt _counter;
    private boolean _ended;
    private AtomicBool _yielded;
    private MArray<T> _array;
    public Future<Seq<T>> future() {
    }
    public Yield<Future<T>> yield() {
    }
    public FutureEnd() {
    }
    static ClassType<FutureEnd<T>> type;
}