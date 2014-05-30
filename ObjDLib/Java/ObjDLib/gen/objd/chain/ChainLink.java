package objd.chain;

import objd.lang.*;

public interface ChainLink<A, B> {
    Yield<A> buildYield(final Yield<B> yield);
    String toString();
}