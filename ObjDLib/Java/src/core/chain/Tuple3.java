package core.chain;

public final final class Tuple3<A, B, C> implements Comparable<Tuple3<A, B, C>> {
    public final A a;
    public final B b;
    public final C c;
    @Override
    public int compareTo(Tuple3<AC, BC, CC> to) {
        int r = to.a.compare(this.a);
        if(r.equals(0)) {
            r = to.b.compare(this.b);
            if(r.equals(0)) {
                return ERROR: Unknown -<l>to\^Tuple3#C<AC#G, BC#G, CC#G>\.<eIU>c\CC#G\.<rdIb>compare(to = <Tuple3#C<A#G, B#G, C#G>>self.<eIU>c\§C#G§\)\int\;
            } else {
                return ERROR: Unknown -<lm>r\int\;
            }
        } else {
            return ERROR: Unknown -<lm>r\int\;
        }
    }
    @Override
    public String toString() {
        return ERROR: Unknown "($<Tuple3#C<A#G, B#G, C#G>>self.<eIU>a\§A#G§\, $<Tuple3#C<A#G, B#G, C#G>>self.<eIU>b\§B#G§\, $<Tuple3#C<A#G, B#G, C#G>>self.<eIU>c\§C#G§\)";
    }
    public static Tuple3<A, B, C> unapplyTuple(Tuple3<A, B, C> tuple) {
        return tuple;
    }
    public Tuple3(A a,B b,C c) {
        this.a = a;
        this.b = b;
        this.c = c;
    }
}