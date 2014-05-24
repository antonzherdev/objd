package objd.lang;

import objd.lang.*;

public final class Tuple5<A, B, C, D, E> implements Comparable<Tuple5<A, B, C, D, E>> {
    public final A a;
    public final B b;
    public final C c;
    public final D d;
    public final E e;
    @Override
    public int compareTo(final Tuple5<A, B, C, D, E> to) {
        int r = ((Comparable<A>)(to.a)).compareTo(this.a);
        if(r == 0) {
            r = ((Comparable<B>)(to.b)).compareTo(this.b);
            if(r == 0) {
                r = ((Comparable<C>)(to.c)).compareTo(this.c);
                if(r == 0) {
                    r = ((Comparable<D>)(to.d)).compareTo(this.d);
                    if(r == 0) {
                        return -(((Comparable<E>)(to.e)).compareTo(this.e));
                    } else {
                        return -(r);
                    }
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
        return String.format("(%s, %s, %s, %s, %s)", this.a, this.b, this.c, this.d, this.e);
    }
    public static <A, B, C, D, E> Tuple5<A, B, C, D, E> unapplyTuple(final Tuple5<A, B, C, D, E> tuple) {
        return tuple;
    }
    public Tuple5(final A a, final B b, final C c, final D d, final E e) {
        this.a = a;
        this.b = b;
        this.c = c;
        this.d = d;
        this.e = e;
    }
}