package core.chain;

public class TreeMapBuilder<K, V> implements Builder<Tuple2<K, V>, TreeMap<K, V>> {
    public final F2<K, K, Integer> comparator;
    private final MTreeMap<K, V> map = new MTreeMap<K, V>(comparator);
    public static TreeMapBuilder<K, V> apply() {
        return new TreeMapBuilder<K, V>(new F2<K, K, Integer>() {
            @Override
            public Integer f(K a,K b) {
                ERROR: Unknown weak return <l>a\K#G\.<rdI>compare(to = <l>b\K#G\)\int\;
            }
        });
    }
    @Override
    public void appendItem(Tuple2<K, V> item) {
        map.appendItem(item);
    }
    @Override
    public TreeMap<K, V> build() {
        return map;
    }
    public TreeMapBuilder(F2<K, K, Integer> comparator) {
    }
    public void appendAllItems(Traversable<T> items) {
        items.forEach(new P<T>() {
            @Override
            public void f(T _) {
                appendItem(_);
            }
        });
    }
}