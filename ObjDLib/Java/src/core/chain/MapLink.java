package core.chain;

public class MapLink<A, B> implements ChainLink<A, B> {
    public final F<A, B> f;
    public Yield<A> buildYield(Yield<B> yield) {
        return ERROR: Unknown <to>Yield\Yield#C.class\.<dIt>decorate(base = <l>yield\Yield#C<§B#G§>\, yield = item : §A#G§ -> int = return <l>yield\Yield#C<§B#G§>\.<dI>yield(item = <MapLink#C<A#G, B#G>>self.<eIU>f\§A#G§ -> §B#G§\.<d>apply( = <l>item\§A#G§\)\§B#G§\)\int\)\Yield#C<§A#G§>\;
    }
    public MapLink(F<A, B> f) {
    }
    static final ClassType<MapLink<A, B>> type;
}