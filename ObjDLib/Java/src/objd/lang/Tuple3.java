package objd.lang;

import objd.lang.*;

public final class Tuple3<A, B, C> implements Comparable<Tuple3<A, B, C>> {
    public final A a;
    public final B b;
    public final C c;
    @Override
    public int compareTo(final Tuple3<A, B, C> to) {
        int r = ((Comparable<A>)(((Comparable)(to.a)))).compareTo(this.a);
        if(r == 0) {
            r = ((Comparable<B>)(((Comparable)(to.b)))).compareTo(this.b);
            if(r == 0) {
                return -(((Comparable<C>)(((Comparable)(to.c)))).compareTo(this.c));
            } else {
                return -(r);
            }
        } else {
            return -(r);
        }
    }
    @Override
    public String toString() {
        return String.format("(%s, %s, %s)", this.a, this.b, this.c);
    }
    public static <A, B, C> Tuple3<A, B, C> unapplyTuple(final Tuple3<A, B, C> tuple) {
        return tuple;
    }
    public Tuple3(final A a, final B b, final C c) {
        this.a = a;
        this.b = b;
        this.c = c;
    }
}