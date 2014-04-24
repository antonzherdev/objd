package core.chain;

public final final class Tuple4<A, B, C, D> implements Comparable<Tuple4<A, B, C, D>> {
    public final A a;
    public final B b;
    public final C c;
    public final D d;
    @Override
    public  <AC extends Comparable<A>, BC extends Comparable<B>, CC extends Comparable<C>, DC extends Comparable<D>> int compareTo(Tuple4<AC, BC, CC, DC> to) {
        int r = to.a.compareTo(this.a);
        if(r.equals(0)) {
            r = to.b.compareTo(this.b);
            if(r.equals(0)) {
                r = to.c.compareTo(this.c);
                if(r.equals(0)) {
                    return -(to.d.compareTo(this.d));
                } else {
                    return -(r);
                }
            } else {
                return -(r);
            }
        } else {
            return -(r);
        }
    }
    @Override
    public String toString() {
        return String.format("(%s, %s, %s, %s)", this.a, this.b, this.c, this.d);
    }
    public static  <A, B, C, D> Tuple4<A, B, C, D> unapplyTuple(Tuple4<A, B, C, D> tuple) {
        return tuple;
    }
    public Tuple4(A a,B b,C c,D d) {
        this.a = a;
        this.b = b;
        this.c = c;
        this.d = d;
    }
}