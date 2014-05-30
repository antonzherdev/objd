package objd.lang;

import objd.lang.*;

public final class Tuple<A, B> implements Comparable<Tuple<A, B>> {
    public final A a;
    public final B b;
    @Override
    public int compareTo(final Tuple<A, B> to) {
        final int r = ((Comparable<A>)(((Comparable)(to.a)))).compareTo(this.a);
        if(r == 0) {
            return -(((Comparable<B>)(((Comparable)(to.b)))).compareTo(this.b));
        } else {
            return -(r);
        }
    }
    @Override
    public String toString() {
        return String.format("(%s, %s)", this.a, this.b);
    }
    public static <A, B> Tuple<A, B> unapplyTuple(final Tuple<A, B> tuple) {
        return tuple;
    }
    public Tuple(final A a, final B b) {
        this.a = a;
        this.b = b;
    }
    public boolean equals(final Object to) {
        if(this == to) {
            return true;
        }
        if(to == null || !(to instanceof Tuple)) {
            return false;
        }
        final Tuple<A, B> o = ((Tuple<A, B>)(((Tuple)(to))));
        return this.a.equals(o.a) && this.b.equals(o.b);
    }
    public int hashCode() {
        int hash = 0;
        hash = hash * 31 + this.a.hashCode();
        hash = hash * 31 + this.b.hashCode();
        return hash;
    }
}