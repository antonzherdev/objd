package core.chain;

public class Tuple3<A, B, C> implements Comparable<Tuple3<A, B, C>> {
    public final A a;
    public final B b;
    public final C c;
    @Override
    public int compareTo(Tuple3<AC, BC, CC> to) {
        ERROR: Unknown local var r : int = <l>to\^Tuple3#C<AC#G, BC#G, CC#G>\.<eIU>a\AC#G\.<rdI>compare(to = <Tuple3#C<A#G, B#G, C#G>>self.<eIU>a\§A#G§\)\int\;
        ERROR: Unknown if((<lm>r\int\ == 0)) {
    (<lm>r\int\ = <l>to\^Tuple3#C<AC#G, BC#G, CC#G>\.<eIU>b\BC#G\.<rdI>compare(to = <Tuple3#C<A#G, B#G, C#G>>self.<eIU>b\§B#G§\)\int\)
    if((<lm>r\int\ == 0)) return -<l>to\^Tuple3#C<AC#G, BC#G, CC#G>\.<eIU>c\CC#G\.<rdI>compare(to = <Tuple3#C<A#G, B#G, C#G>>self.<eIU>c\§C#G§\)\int\
else return -<lm>r\int\
}
else return -<lm>r\int\;
    }
    @Override
    public String description() {
        return ERROR: Unknown "($<Tuple3#C<A#G, B#G, C#G>>self.<eIU>a\§A#G§\, $<Tuple3#C<A#G, B#G, C#G>>self.<eIU>b\§B#G§\, $<Tuple3#C<A#G, B#G, C#G>>self.<eIU>c\§C#G§\)";
    }
    public static Tuple3<A, B, C> unapplyTuple(Tuple3<A, B, C> tuple) {
        return ERROR: Unknown some(<l>tuple\Tuple3#C<A#G, B#G, C#G>\)\(^Tuple3#C<A#G, B#G, C#G>)?\;
    }
    public Tuple3(A a,B b,C c) {
    }
}