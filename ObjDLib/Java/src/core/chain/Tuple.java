package core.chain;

public class Tuple<A, B> implements Comparable<Tuple<A, B>> {
    public final A a;
    public final B b;
    @Override
    public int compareTo(Tuple<AC, BC> to) {
        ERROR: Unknown local r : int = <l>to\^Tuple#C<AC#G, BC#G>\.<eIU>a\AC#G\.<rdI>compare(to = <Tuple#C<A#G, B#G>>self.<eIU>a\§A#G§\)\int\;
        if(r.equals(ERROR: Unknown 0)) {
            return ERROR: Unknown -<l>to\^Tuple#C<AC#G, BC#G>\.<eIU>b\BC#G\.<rdI>compare(to = <Tuple#C<A#G, B#G>>self.<eIU>b\§B#G§\)\int\;
        } else {
            return ERROR: Unknown -<l>r\int\;
        }
    }
    @Override
    public String description() {
        return ERROR: Unknown "($<Tuple#C<A#G, B#G>>self.<eIU>a\§A#G§\, $<Tuple#C<A#G, B#G>>self.<eIU>b\§B#G§\)";
    }
    public static Tuple<A, B> unapplyTuple(Tuple<A, B> tuple) {
        return tuple;
    }
    public Tuple(A a,B b) {
    }
}