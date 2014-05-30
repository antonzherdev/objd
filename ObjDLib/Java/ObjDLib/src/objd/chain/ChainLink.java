package objd.chain;

public interface ChainLink<A, B> {
    Yield<A> buildYield(final Yield<B> yield);
    String toString();
}