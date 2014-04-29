package objd.lang;

public final class Tuple3<A, B, C> implements Comparable<Tuple3<A, B, C>> {
    public final A a;
    public final B b;
    public final C c;
    @Override
    public <AC extends Comparable<A>, BC extends Comparable<B>, CC extends Comparable<C>> int compareTo(final Tuple3<AC, BC, CC> to) {
        int r = to.a.compareTo(this.a);
        if(r == 0) {
            r = to.b.compareTo(this.b);
            if(r == 0) {
                return -(to.c.compareTo(this.c));
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