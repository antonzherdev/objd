package core.chain;

public class ImTreeMap<K, V> extends TreeMap<K, V> {
    public final TreeMapEntry<K, V> root;
    public final int count;
    public final TreeMapKeySet<K> keys = new ImTreeMapKeySet<K>(ERROR: Unknown <ImTreeMap#C<K#G, V#G>>self);
    public boolean isEmpty() {
        return ERROR: Unknown (<ImTreeMap#C<K#G, V#G>>self.<eIUo>root\(^TreeMapEntry#C<§K#G§, §V#G§>)?\ == none<^TreeMapEntry#C<§K#G§, §V#G§>>);
    }
    public MTreeMap<K, V> mCopy() {
        ERROR: Unknown local m : MTreeMap#C<§K#G§, §V#G§> = <to>MTreeMap\MTreeMap#C.class\.<tcI>apply(comparator = <ImTreeMap#C<K#G, V#G>>self.<reIU>comparator\(§K#G§, §K#G§) -> int\)\MTreeMap#C<§K#G§, §V#G§>\;
        m.assignImMap(ERROR: Unknown <ImTreeMap#C<K#G, V#G>>self);
        return m;
    }
    public ImTreeMap(F2<K, K, Integer> comparator,TreeMapEntry<K, V> root,int count) {
    }
    static final ClassType<ImTreeMap<K, V>> type;
}