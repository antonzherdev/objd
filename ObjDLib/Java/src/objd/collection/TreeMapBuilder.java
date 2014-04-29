package objd.collection;

public class TreeMapBuilder<K, V> extends Builder_impl<Tuple<K, V>, TreeMap<K, V>> {
    public final F2<K, K, Integer> comparator;
    private final MTreeMap<K, V> map;
    public static <K extends Comparable<K>, V> TreeMapBuilder<K, V> apply() {
        return new TreeMapBuilder<K, V>(new F2<K, K, Integer>() {
            @Override
            public Integer apply(final K a, final K b) {
                return a.compareTo(b);
            }
        });
    }
    @Override
    public void appendItem(final Tuple<K, V> item) {
        this.map.appendItem(item);
    }
    @Override
    public TreeMap<K, V> build() {
        return this.map;
    }
    public TreeMapBuilder(final F2<K, K, Integer> comparator) {
        this.comparator = comparator;
        this.map = new MTreeMap<K, V>(comparator);
    }
}