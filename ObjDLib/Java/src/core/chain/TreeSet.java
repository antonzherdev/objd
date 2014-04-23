package core.chain;

public class TreeSet<T> extends Set_impl<T> {
    public final TreeMap<T, Object> map;
    public T higherThanItem(T item) {
        return this.map.higherKeyThanKey(item);
    }
    public T lowerThanItem(T item) {
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
    public Iterator<T> iteratorHigherThanItem(T item) {
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
    public boolean containsItem(T item) {
        return this.map.containsKey(item);
    }
    public TreeSet(TreeMap<T, Object> map) {
        this.map = map;
    }
}