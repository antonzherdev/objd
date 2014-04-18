package core.chain;

public class FutureVoidEnd<T> {
    private Promise<Void> _promise;
    private boolean _stopped;
    private AtomicInt _counter;
    private boolean _ended;
    private AtomicBool _yielded;
    public Future<Void> future() {
    }
    public Yield<Future<T>> yield() {
    }
    public FutureVoidEnd() {
    }
    static ClassType<FutureVoidEnd<T>> type;
}