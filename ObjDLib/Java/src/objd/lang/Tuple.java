package objd.lang;

public final class Tuple<A, B> implements Comparable<Tuple<A, B>> {
    public final A a;
    public final B b;
    @Override
    public <AC extends Comparable<A>, BC extends Comparable<B>> int compareTo(final Tuple<AC, BC> to) {
        final int r = to.a.compareTo(this.a);
        if(r == 0) {
            return -(to.b.compareTo(this.b));
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
}