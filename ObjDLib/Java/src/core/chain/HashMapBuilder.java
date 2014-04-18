package core.chain;

public class HashMapBuilder<K, V> implements Builder<Tuple2<K, V>, ImHashMap<K, V>> {
    private final MHashMap<K, V> map = new MHashMap();
    public void appendItem(Tuple2<K, V> item) {
        ERROR: Unknown <HashMapBuilder#C<K#G, V#G>>self.<ep>map\MHashMap#C<§K#G§, §V#G§>\.<rdIa>set(key = <l>item\^(§K#G§, §V#G§)\.<eIU>a\§K#G§\, value = <l>item\^(§K#G§, §V#G§)\.<eIU>b\§V#G§\)\void\;
    }
    public ImHashMap<K, V> build() {
        return ERROR: Unknown <HashMapBuilder#C<K#G, V#G>>self.<ep>map\MHashMap#C<§K#G§, §V#G§>\.<dIo>im\ImHashMap#C<§K#G§, §V#G§>\;
    }
    public HashMapBuilder() {
    }
    static final ClassType<HashMapBuilder<K, V>> type;
    public void appendAllItems(Traversable<T> items) {
        ERROR: Unknown <l>items\Traversable#T<§T#G§>\.<dI>for(each = _ : §T#G§ -> void = <Builder#T<T#G, C#G>>self.<dIa>append(item = <l>_\§T#G§\)\void\)\void\;
    }
}