package core.chain;

public class ImTreeMap<K, V> extends TreeMap<K, V> {
    public final TreeMapEntry<K, V> root;
    public final int count;
    public final TreeMapKeySet<K> keys = new ImTreeMapKeySet<K>(this);
    @Override
    public boolean isEmpty() {
        return this.root == null;
    }
    @Override
    public MTreeMap<K, V> mCopy() {
        ERROR: Unknown local m : MTreeMap#C<§K#G§, §V#G§> = <to>MTreeMap\MTreeMap#C.class\.<tcI>apply(comparator = <ImTreeMap#C<K#G, V#G>>self.<reIU>comparator\(§K#G§, §K#G§) -> int\)\MTreeMap#C<§K#G§, §V#G§>\;
        m.assignImMap(this);
        return m;
    }
    public ImTreeMap(F2<K, K, Integer> comparator,TreeMapEntry<K, V> root,int count) {
        this.comparator = comparator;
        this.root = root;
        this.count = count;
    }
}