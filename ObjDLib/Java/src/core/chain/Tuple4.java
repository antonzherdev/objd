package core.chain;

public class Tuple4<A, B, C, D> implements Comparable<Tuple4<A, B, C, D>> {
    public final A a;
    public final B b;
    public final C c;
    public final D d;
    @Override
    public int compareTo(Tuple4<AC, BC, CC, DC> to) {
        ERROR: Unknown local var r : int = <l>to\^Tuple4#C<AC#G, BC#G, CC#G, DC#G>\.<eIU>a\AC#G\.<rdIb>compare(to = <Tuple4#C<A#G, B#G, C#G, D#G>>self.<eIU>a\§A#G§\)\int\;
        if(r.equals(ERROR: Unknown 0)) {
            r = to.b.compareTo(this.b);
            if(r.equals(ERROR: Unknown 0)) {
                r = to.c.compareTo(this.c);
                if(r.equals(ERROR: Unknown 0)) {
                    return ERROR: Unknown -<l>to\^Tuple4#C<AC#G, BC#G, CC#G, DC#G>\.<eIU>d\DC#G\.<rdIb>compare(to = <Tuple4#C<A#G, B#G, C#G, D#G>>self.<eIU>d\§D#G§\)\int\;
                } else {
                    return ERROR: Unknown -<lm>r\int\;
                }
            } else {
                return ERROR: Unknown -<lm>r\int\;
            }
        } else {
            return ERROR: Unknown -<lm>r\int\;
        }
    }
    @Override
    public String description() {
        return ERROR: Unknown "($<Tuple4#C<A#G, B#G, C#G, D#G>>self.<eIU>a\§A#G§\, $<Tuple4#C<A#G, B#G, C#G, D#G>>self.<eIU>b\§B#G§\, $<Tuple4#C<A#G, B#G, C#G, D#G>>self.<eIU>c\§C#G§\, $<Tuple4#C<A#G, B#G, C#G, D#G>>self.<eIU>d\§D#G§\)";
    }
    public static Tuple4<A, B, C, D> unapplyTuple(Tuple4<A, B, C, D> tuple) {
        return tuple;
    }
    public Tuple4(A a,B b,C c,D d) {
        this.a = a;
        this.b = b;
        this.c = c;
        this.d = d;
    }
}