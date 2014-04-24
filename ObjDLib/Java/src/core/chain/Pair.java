package core.chain;

public final class Pair<T> extends ImSet_impl<T> {
    public final T a;
    public final T b;
    public static  <T> Pair<T> newWithAB(T a,T b) {
        if(a < b) {
            return new Pair<T>(a, b);
        } else {
            return new Pair<T>(b, a);
        }
    }
    @Override
    public boolean containsItem(T item) {
        return this.a.equals(item) || this.b.equals(item);
    }
    @Override
    public int count() {
        return ((int)2);
    }
    @Override
    public Iterator<T> iterator() {
        return new PairIterator<T>(this);
    }
    @Override
    public T head() {
        return this.a;
    }
    public Pair(T a,T b) {
        this.a = a;
        this.b = b;
    }
}