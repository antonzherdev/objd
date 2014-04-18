package core.chain;

public class MapLink<A, B> implements ChainLink<A, B> {
    public final F<A, B> f;
    public Yield<A> buildYield(Yield<B> yield) {
        return Yield().decorateBaseYield<A>(yield, ERROR: Unknown item : §A#G§ -> int = return <l>yield\Yield#C<§B#G§>\.<dI>yield(item = <MapLink#C<A#G, B#G>>self.<eIU>f\§A#G§ -> §B#G§\.<d>apply( = <l>item\§A#G§\)\§B#G§\)\int\);
    }
    public MapLink(F<A, B> f) {
    }
    static final ClassType<MapLink<A, B>> type;
}