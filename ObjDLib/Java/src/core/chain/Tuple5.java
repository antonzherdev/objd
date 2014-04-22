package core.chain;

public class Tuple5<A, B, C, D, E> implements Comparable<Tuple5<A, B, C, D, E>> {
    public final A a;
    public final B b;
    public final C c;
    public final D d;
    public final E e;
    @Override
    public int compareTo(Tuple5<AC, BC, CC, DC, EC> to) {
        int r = to.a.compareTo(this.a);
        if(r.equals(ERROR: Unknown 0)) {
            r = to.b.compareTo(this.b);
            if(r.equals(ERROR: Unknown 0)) {
                r = to.c.compareTo(this.c);
                if(r.equals(ERROR: Unknown 0)) {
                    r = to.d.compareTo(this.d);
                    if(r.equals(ERROR: Unknown 0)) {
                        return ERROR: Unknown -<l>to\^Tuple5#C<AC#G, BC#G, CC#G, DC#G, EC#G>\.<eIU>e\EC#G\.<rdIb>compare(to = <Tuple5#C<A#G, B#G, C#G, D#G, E#G>>self.<eIU>e\§E#G§\)\int\;
                    } else {
                        return ERROR: Unknown -<lm>r\int\;
                    }
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
        return ERROR: Unknown "($<Tuple5#C<A#G, B#G, C#G, D#G, E#G>>self.<eIU>a\§A#G§\, $<Tuple5#C<A#G, B#G, C#G, D#G, E#G>>self.<eIU>b\§B#G§\, $<Tuple5#C<A#G, B#G, C#G, D#G, E#G>>self.<eIU>c\§C#G§\, $<Tuple5#C<A#G, B#G, C#G, D#G, E#G>>self.<eIU>d\§D#G§\, $<Tuple5#C<A#G, B#G, C#G, D#G, E#G>>self.<eIU>e\§E#G§\)";
    }
    public static Tuple5<A, B, C, D, E> unapplyTuple(Tuple5<A, B, C, D, E> tuple) {
        return tuple;
    }
    public Tuple5(A a,B b,C c,D d,E e) {
        this.a = a;
        this.b = b;
        this.c = c;
        this.d = d;
        this.e = e;
    }
}