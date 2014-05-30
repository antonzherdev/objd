package objd.lang;

public final class Tuple4<A, B, C, D> implements Comparable<Tuple4<A, B, C, D>> {
    public final A a;
    public final B b;
    public final C c;
    public final D d;
    @Override
    public int compareTo(final Tuple4<A, B, C, D> to) {
        int r = ((Comparable<A>)(((Comparable)(to.a)))).compareTo(this.a);
        if(r == 0) {
            r = ((Comparable<B>)(((Comparable)(to.b)))).compareTo(this.b);
            if(r == 0) {
                r = ((Comparable<C>)(((Comparable)(to.c)))).compareTo(this.c);
                if(r == 0) {
                    return -(((Comparable<D>)(((Comparable)(to.d)))).compareTo(this.d));
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
    public static <A, B, C, D> Tuple4<A, B, C, D> unapplyTuple(final Tuple4<A, B, C, D> tuple) {
        return tuple;
    }
    public Tuple4(final A a, final B b, final C c, final D d) {
        this.a = a;
        this.b = b;
        this.c = c;
        this.d = d;
    }
    public boolean equals(final Object to) {
        if(this == to) {
            return true;
        }
        if(to == null || !(to instanceof Tuple4)) {
            return false;
        }
        final Tuple4<A, B, C, D> o = ((Tuple4<A, B, C, D>)(((Tuple4)(to))));
        return this.a.equals(o.a) && this.b.equals(o.b) && this.c.equals(o.c) && this.d.equals(o.d);
    }
    public int hashCode() {
        int hash = 0;
        hash = hash * 31 + this.a.hashCode();
        hash = hash * 31 + this.b.hashCode();
        hash = hash * 31 + this.c.hashCode();
        hash = hash * 31 + this.d.hashCode();
        return hash;
    }
}