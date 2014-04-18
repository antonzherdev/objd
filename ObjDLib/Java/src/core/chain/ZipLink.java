package core.chain;

public class ZipLink<T, A, R> implements ChainLink<T, R> {
    public Iterable<A> a;
    public F2<T, A, R> f;
    public Yield<A> buildYield(Yield<R> yield) {
    }
    public ZipLink(Iterable<A> a,F2<T, A, R> f) {
    }
    static ClassType<ZipLink<T, A, R>> type;
}