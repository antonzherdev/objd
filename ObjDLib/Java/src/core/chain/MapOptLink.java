package core.chain;

public class MapOptLink<A, B> implements ChainLink<A, B> {
    public F<A, B> f;
    public Yield<A> buildYield(Yield<B> yield) {
    }
    public MapOptLink(F<A, B> f) {
    }
    static ClassType<MapOptLink<A, B>> type;
}