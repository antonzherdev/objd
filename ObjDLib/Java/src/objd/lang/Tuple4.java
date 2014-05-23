package objd.lang;

import objd.lang.*;

public final class Tuple4<A, B, C, D> implements Comparable<Tuple4<A, B, C, D>> {
    public final A a;
    public final B b;
    public final C c;
    public final D d;
    @Override
    public int compareTo(final Tuple4<A, B, C, D> to) {
        int r = ((Comparable<A>)(to.a)).compareTo(this.a);
        if(r == 0) {
            r = ((Comparable<B>)(to.b)).compareTo(this.b);
            if(r == 0) {
                r = ((Comparable<C>)(to.c)).compareTo(this.c);
                if(r == 0) {
                    return -(((Comparable<D>)(to.d)).compareTo(this.d));
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
}