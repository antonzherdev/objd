package core.chain;

public class Cache<X, F> {
    public final F<X, F> f;
    private X _lastX;
    private F _lastF;
    public F applyX(X x) {
        ERROR: Unknown if(((<Cache#C<X#G, F#G>>self.<emp>_lastX\(§X#G§)?\ != none<§X#G§>) && (<Cache#C<X#G, F#G>>self.<emp>_lastX\(§X#G§)?\ == <l>x\§X#G§\))) return <Cache#C<X#G, F#G>>self.<emp>_lastF\(§F#G§)?\.get
else {
    (<Cache#C<X#G, F#G>>self.<emp>_lastX\(§X#G§)?\ = some(<l>x\§X#G§\)\§(X#G)?§\)
    (<Cache#C<X#G, F#G>>self.<emp>_lastF\(§F#G§)?\ = some(<Cache#C<X#G, F#G>>self.<eIU>f\§X#G§ -> §F#G§\.<d>apply( = <l>x\§X#G§\)\§F#G§\)\§(F#G)?§\)
    return <Cache#C<X#G, F#G>>self.<emp>_lastF\(§F#G§)?\.get
};
    }
    public Cache(F<X, F> f) {
    }
}