package objd.collection;

public class TreeSet<T> extends Set_impl<T> {
    public final TreeMap<T, Object> map;
    public T higherThanItem(final T item) {
        return this.map.higherKeyThanKey(item);
    }
    public T lowerThanItem(final T item) {
        return this.map.lowerKeyThanKey(item);
    }
    @Override
    public int count() {
        return this.map.count();
    }
    @Override
    public Iterator<T> iterator() {
        return this.map.keys().iterator();
    }
    public Iterator<T> iteratorHigherThanItem(final T item) {
        return this.map.keys().iteratorHigherThanKey(item);
    }
    @Override
    public T head() {
        return this.map.firstKey();
    }
    public T last() {
        return this.map.lastKey();
    }
    @Override
    public boolean containsItem(final T item) {
        return this.map.containsKey(item);
    }
    public TreeSet(final TreeMap<T, Object> map) {
        this.map = map;
    }
    public String toString() {
        return String.format("TreeSet(%s)", this.map);
    }
}