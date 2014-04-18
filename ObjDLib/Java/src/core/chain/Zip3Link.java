package core.chain;

public class Zip3Link<T, A, B, R> implements ChainLink<T, R> {
    public Iterable<A> a;
    public Iterable<B> b;
    public F3<T, A, B, R> f;
    public Yield<A> buildYield(Yield<R> yield) {
    }
    public Zip3Link(Iterable<A> a,Iterable<B> b,F3<T, A, B, R> f) {
    }
    static ClassType<Zip3Link<T, A, B, R>> type;
}