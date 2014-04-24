package core.chain;

public class TreeMapBuilder<K, V> extends Builder_impl<Tuple2<K, V>, TreeMap<K, V>> {
    public final F2<K, K, Integer> comparator;
    private final MTreeMap<K, V> map;
    public static  <K extends Comparable<K>, V> TreeMapBuilder<K, V> apply() {
        return new TreeMapBuilder<K, V>(new F2<K, K, Integer>() {
            @Override
            public Integer apply(K a,K b) {
                ERROR: Unknown weak return <l>a\K#G\.<rdIb>compare(to = <l>b\K#G\)\int\;
            }
        });
    }
    @Override
    public void appendItem(Tuple2<K, V> item) {
        this.map.appendItem(item);
    }
    @Override
    public TreeMap<K, V> build() {
        return this.map;
    }
    public TreeMapBuilder(F2<K, K, Integer> comparator) {
        this.comparator = comparator;
        this.map = new MTreeMap<K, V>(comparator);
    }
}