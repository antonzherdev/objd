package core.chain;

public class MapOptLink<A, B> implements ChainLink<A, B> {
    public final F<A, B> f;
    public Yield<A> buildYield(Yield<B> yield) {
        return Yield().decorateBaseYield<A>(yield, ERROR: Unknown item : §A#G§ -> int = {
    val __tmp_0 : ^(^int)?
{
    local _ : §(B#G)¿§ = <MapOptLink#C<A#G, B#G>>self.<eIU>f\§A#G§ -> (§B#G§)?\.<d>apply( = <l>item\§A#G§\)\(§B#G§)?\
    if((<l>_\§(B#G)¿§\ != none<§B#G§>)) (<l>__tmp_0\^(^int)?\ = some(<l>yield\Yield#C<§B#G§>\.<dI>yield(item = <l>_\§(B#G)¿§\)\int\)\(^int)?\)
else (<l>__tmp_0\^(^int)?\ = none<^int>)
}
    if((<l>__tmp_0\^(^int)?\ != none<^int>)) return <l>__tmp_0\^(^int)?\.get
else return 0
});
    }
    public MapOptLink(F<A, B> f) {
    }
    static final ClassType<MapOptLink<A, B>> type;
}