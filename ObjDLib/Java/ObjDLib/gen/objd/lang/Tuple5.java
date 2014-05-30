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
        int r = ((Comparable<A>)(((Comparable)(to.a)))).compareTo(this.a);
        if(r == 0) {
            r = ((Comparable<B>)(((Comparable)(to.b)))).compareTo(this.b);
            if(r == 0) {
                r = ((Comparable<C>)(((Comparable)(to.c)))).compareTo(this.c);
                if(r == 0) {
                    r = ((Comparable<D>)(((Comparable)(to.d)))).compareTo(this.d);
                    if(r == 0) {
                        return -(((Comparable<E>)(((Comparable)(to.e)))).compareTo(this.e));
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
    public boolean equals(final Object to) {
        if(this == to) {
            return true;
        }
        if(to == null || !(to instanceof Tuple5)) {
            return false;
        }
        final Tuple5<A, B, C, D, E> o = ((Tuple5<A, B, C, D, E>)(((Tuple5)(to))));
        return this.a.equals(o.a) && this.b.equals(o.b) && this.c.equals(o.c) && this.d.equals(o.d) && this.e.equals(o.e);
    }
    public int hashCode() {
        int hash = 0;
        hash = hash * 31 + this.a.hashCode();
        hash = hash * 31 + this.b.hashCode();
        hash = hash * 31 + this.c.hashCode();
        hash = hash * 31 + this.d.hashCode();
        hash = hash * 31 + this.e.hashCode();
        return hash;
    }
}