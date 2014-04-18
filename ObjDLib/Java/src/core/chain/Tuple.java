package core.chain;

public class Tuple<A, B> implements Comparable<Tuple<A, B>> {
    public final A a;
    public final B b;
    public int compareTo(Tuple<AC, BC> to) {
        ERROR: Unknown local r : int = <l>to\^Tuple#C<AC#G, BC#G>\.<eIU>a\AC#G\.<rdI>compare(to = <Tuple#C<A#G, B#G>>self.<eIU>a\§A#G§\)\int\;
        ERROR: Unknown if((<l>r\int\ == 0)) return -<l>to\^Tuple#C<AC#G, BC#G>\.<eIU>b\BC#G\.<rdI>compare(to = <Tuple#C<A#G, B#G>>self.<eIU>b\§B#G§\)\int\
else return -<l>r\int\;
    }
    public String description() {
        return ERROR: Unknown "($<Tuple#C<A#G, B#G>>self.<eIU>a\§A#G§\, $<Tuple#C<A#G, B#G>>self.<eIU>b\§B#G§\)";
    }
    public static Tuple<A, B> unapplyTuple(Tuple<A, B> tuple) {
        return ERROR: Unknown some(<l>tuple\Tuple#C<A#G, B#G>\)\(^Tuple#C<A#G, B#G>)?\;
    }
    public Tuple(A a,B b) {
    }
    static final ClassType<Tuple<A, B>> type;
}