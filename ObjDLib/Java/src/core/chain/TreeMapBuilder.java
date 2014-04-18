package core.chain;

public class TreeMapBuilder<K, V> implements Builder<Tuple2<K, V>, TreeMap<K, V>> {
    public final F2<K, K, Integer> comparator;
    private final MTreeMap<K, V> map = new MTreeMap<K, V>(comparator);
    public static TreeMapBuilder<K, V> apply() {
        return new TreeMapBuilder<K, V>(ERROR: Unknown a : K#G, b : K#G -> int = weak return <l>a\K#G\.<rdI>compare(to = <l>b\K#G\)\int\);
    }
    public void appendItem(Tuple2<K, V> item) {
        map.appendItem(item);
    }
    public TreeMap<K, V> build() {
        return map;
    }
    public TreeMapBuilder(F2<K, K, Integer> comparator) {
    }
    static final ClassType<TreeMapBuilder<K, V>> type;
    public void appendAllItems(Traversable<T> items) {
        items.forEach(ERROR: Unknown _ : §T#G§ -> void = <Builder#T<T#G, C#G>>self.<dIa>append(item = <l>_\§T#G§\)\void\);
    }
}