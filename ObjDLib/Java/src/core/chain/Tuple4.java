package core.chain;

public class Tuple4<A, B, C, D> implements Comparable<Tuple4<A, B, C, D>> {
    public A a;
    public B b;
    public C c;
    public D d;
    public int compareTo(Tuple4<AC, BC, CC, DC> to) {
    }
    public String description() {
    }
    public static Tuple4<A, B, C, D> unapplyTuple(Tuple4<A, B, C, D> tuple) {
    }
    public Tuple4(A a,B b,C c,D d) {
    }
    static ClassType<Tuple4<A, B, C, D>> type;
}