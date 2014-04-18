package core.chain;

public class TreeMapBuilder<K, V> implements Builder<Tuple2<K, V>, TreeMap<K, V>> {
    public F2<K, K, Integer> comparator;
    private MTreeMap<K, V> map;
    public static TreeMapBuilder<K, V> apply() {
    }
    public void appendItem(Tuple2<K, V> item) {
    }
    public TreeMap<K, V> build() {
    }
    public TreeMapBuilder(F2<K, K, Integer> comparator) {
    }
    static ClassType<TreeMapBuilder<K, V>> type;
    public void appendAllItems(Traversable<T> items) {
    }
}