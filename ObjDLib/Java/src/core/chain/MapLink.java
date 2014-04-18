package core.chain;

public class MapLink<A, B> implements ChainLink<A, B> {
    public F<A, B> f;
    public Yield<A> buildYield(Yield<B> yield) {
    }
    public MapLink(F<A, B> f) {
    }
    static ClassType<MapLink<A, B>> type;
}