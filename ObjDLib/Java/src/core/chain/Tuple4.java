package core.chain;

public class Tuple4<A, B, C, D> implements Comparable<Tuple4<A, B, C, D>> {
    public final A a;
    public final B b;
    public final C c;
    public final D d;
    public int compareTo(Tuple4<AC, BC, CC, DC> to) {
        ERROR: Unknown local var r : int = <l>to\^Tuple4#C<AC#G, BC#G, CC#G, DC#G>\.<eIU>a\AC#G\.<rdI>compare(to = <Tuple4#C<A#G, B#G, C#G, D#G>>self.<eIU>a\§A#G§\)\int\;
        ERROR: Unknown if((<lm>r\int\ == 0)) {
    (<lm>r\int\ = <l>to\^Tuple4#C<AC#G, BC#G, CC#G, DC#G>\.<eIU>b\BC#G\.<rdI>compare(to = <Tuple4#C<A#G, B#G, C#G, D#G>>self.<eIU>b\§B#G§\)\int\)
    if((<lm>r\int\ == 0)) {
    (<lm>r\int\ = <l>to\^Tuple4#C<AC#G, BC#G, CC#G, DC#G>\.<eIU>c\CC#G\.<rdI>compare(to = <Tuple4#C<A#G, B#G, C#G, D#G>>self.<eIU>c\§C#G§\)\int\)
    if((<lm>r\int\ == 0)) return -<l>to\^Tuple4#C<AC#G, BC#G, CC#G, DC#G>\.<eIU>d\DC#G\.<rdI>compare(to = <Tuple4#C<A#G, B#G, C#G, D#G>>self.<eIU>d\§D#G§\)\int\
else return -<lm>r\int\
}
else return -<lm>r\int\
}
else return -<lm>r\int\;
    }
    public String description() {
        return ERROR: Unknown "($<Tuple4#C<A#G, B#G, C#G, D#G>>self.<eIU>a\§A#G§\, $<Tuple4#C<A#G, B#G, C#G, D#G>>self.<eIU>b\§B#G§\, $<Tuple4#C<A#G, B#G, C#G, D#G>>self.<eIU>c\§C#G§\, $<Tuple4#C<A#G, B#G, C#G, D#G>>self.<eIU>d\§D#G§\)";
    }
    public static Tuple4<A, B, C, D> unapplyTuple(Tuple4<A, B, C, D> tuple) {
        return ERROR: Unknown some(<l>tuple\Tuple4#C<A#G, B#G, C#G, D#G>\)\(^Tuple4#C<A#G, B#G, C#G, D#G>)?\;
    }
    public Tuple4(A a,B b,C c,D d) {
    }
    static final ClassType<Tuple4<A, B, C, D>> type;
}