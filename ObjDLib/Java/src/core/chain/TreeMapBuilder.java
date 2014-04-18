package core.chain;

public class TreeMapBuilder<K, V> implements Builder<Tuple2<K, V>, TreeMap<K, V>> {
    public final F2<K, K, Integer> comparator;
    private final MTreeMap<K, V> map = new MTreeMap(ERROR: Unknown <TreeMapBuilder#C<K#G, V#G>>self.<eIU>comparator\(§K#G§, §K#G§) -> int\);
    public static TreeMapBuilder<K, V> apply() {
        return new TreeMapBuilder(ERROR: Unknown a : K#G, b : K#G -> int = weak return <l>a\K#G\.<rdI>compare(to = <l>b\K#G\)\int\);
    }
    public void appendItem(Tuple2<K, V> item) {
        ERROR: Unknown <TreeMapBuilder#C<K#G, V#G>>self.<ep>map\MTreeMap#C<§K#G§, §V#G§>\.<rdIo>append(item = <l>item\^(§K#G§, §V#G§)\)\void\;
    }
    public TreeMap<K, V> build() {
        return ERROR: Unknown <TreeMapBuilder#C<K#G, V#G>>self.<ep>map\MTreeMap#C<§K#G§, §V#G§>\;
    }
    public TreeMapBuilder(F2<K, K, Integer> comparator) {
    }
    static final ClassType<TreeMapBuilder<K, V>> type;
    public void appendAllItems(Traversable<T> items) {
        ERROR: Unknown <l>items\Traversable#T<§T#G§>\.<dI>for(each = _ : §T#G§ -> void = <Builder#T<T#G, C#G>>self.<dIa>append(item = <l>_\§T#G§\)\void\)\void\;
    }
}