package core.chain;

public final final class Tuple<A, B> implements Comparable<Tuple<A, B>> {
    public final A a;
    public final B b;
    @Override
    public int compareTo(Tuple<AC, BC> to) {
        int r = to.a.compareTo(this.a);
        if(r.equals(0)) {
            return ERROR: Unknown -<l>to\^Tuple#C<AC#G, BC#G>\.<eIU>b\BC#G\.<rdIb>compare(to = <Tuple#C<A#G, B#G>>self.<eIU>b\§B#G§\)\int\;
        } else {
            return ERROR: Unknown -<l>r\int\;
        }
    }
    @Override
    public String toString() {
        return ERROR: Unknown "($<Tuple#C<A#G, B#G>>self.<eIU>a\§A#G§\, $<Tuple#C<A#G, B#G>>self.<eIU>b\§B#G§\)";
    }
    public static Tuple<A, B> unapplyTuple(Tuple<A, B> tuple) {
        return tuple;
    }
    public Tuple(A a,B b) {
        this.a = a;
        this.b = b;
    }
}