package core.chain;

public class Tuple5<A, B, C, D, E> implements Comparable<Tuple5<A, B, C, D, E>> {
    public final A a;
    public final B b;
    public final C c;
    public final D d;
    public final E e;
    @Override
    public int compareTo(Tuple5<AC, BC, CC, DC, EC> to) {
        ERROR: Unknown local var r : int = <l>to\^Tuple5#C<AC#G, BC#G, CC#G, DC#G, EC#G>\.<eIU>a\AC#G\.<rdI>compare(to = <Tuple5#C<A#G, B#G, C#G, D#G, E#G>>self.<eIU>a\§A#G§\)\int\;
        ERROR: Unknown if((<lm>r\int\ == 0)) {
    (<lm>r\int\ = <l>to\^Tuple5#C<AC#G, BC#G, CC#G, DC#G, EC#G>\.<eIU>b\BC#G\.<rdI>compare(to = <Tuple5#C<A#G, B#G, C#G, D#G, E#G>>self.<eIU>b\§B#G§\)\int\)
    if((<lm>r\int\ == 0)) {
    (<lm>r\int\ = <l>to\^Tuple5#C<AC#G, BC#G, CC#G, DC#G, EC#G>\.<eIU>c\CC#G\.<rdI>compare(to = <Tuple5#C<A#G, B#G, C#G, D#G, E#G>>self.<eIU>c\§C#G§\)\int\)
    if((<lm>r\int\ == 0)) {
    (<lm>r\int\ = <l>to\^Tuple5#C<AC#G, BC#G, CC#G, DC#G, EC#G>\.<eIU>d\DC#G\.<rdI>compare(to = <Tuple5#C<A#G, B#G, C#G, D#G, E#G>>self.<eIU>d\§D#G§\)\int\)
    if((<lm>r\int\ == 0)) return -<l>to\^Tuple5#C<AC#G, BC#G, CC#G, DC#G, EC#G>\.<eIU>e\EC#G\.<rdI>compare(to = <Tuple5#C<A#G, B#G, C#G, D#G, E#G>>self.<eIU>e\§E#G§\)\int\
else return -<lm>r\int\
}
else return -<lm>r\int\
}
else return -<lm>r\int\
}
else return -<lm>r\int\;
    }
    @Override
    public String description() {
        return ERROR: Unknown "($<Tuple5#C<A#G, B#G, C#G, D#G, E#G>>self.<eIU>a\§A#G§\, $<Tuple5#C<A#G, B#G, C#G, D#G, E#G>>self.<eIU>b\§B#G§\, $<Tuple5#C<A#G, B#G, C#G, D#G, E#G>>self.<eIU>c\§C#G§\, $<Tuple5#C<A#G, B#G, C#G, D#G, E#G>>self.<eIU>d\§D#G§\, $<Tuple5#C<A#G, B#G, C#G, D#G, E#G>>self.<eIU>e\§E#G§\)";
    }
    public static Tuple5<A, B, C, D, E> unapplyTuple(Tuple5<A, B, C, D, E> tuple) {
        return ERROR: Unknown some(<l>tuple\Tuple5#C<A#G, B#G, C#G, D#G, E#G>\)\(^Tuple5#C<A#G, B#G, C#G, D#G, E#G>)?\;
    }
    public Tuple5(A a,B b,C c,D d,E e) {
    }
}