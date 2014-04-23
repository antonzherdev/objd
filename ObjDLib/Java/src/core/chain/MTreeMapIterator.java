package core.chain;

public class MTreeMapIterator<K, V> extends MIterator_impl<Tuple2<K, V>> {
    public final MTreeMap<K, V> map;
    private TreeMapEntry<K, V> prev;
    public TreeMapEntry<K, V> entry;
    public static MTreeMapIterator<K, V> applyMapEntry(MTreeMap<K, V> map,TreeMapEntry<K, V> entry) {
        MTreeMapIterator<K, V> ret = new MTreeMapIterator<K, V>(map);
        ret.entry = entry;
        return ret;
    }
    @Override
    public boolean hasNext() {
        return this.entry != null;
    }
    @Override
    public Tuple2<K, V> next() {
        Tuple2<K, V> ret = ERROR: Unknown (<MTreeMapIterator#C<K#G, V#G>>self.<eIm>entry\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIUm>key\§K#G§\, <MTreeMapIterator#C<K#G, V#G>>self.<eIm>entry\(^TreeMapEntry#C<§K#G§, §V#G§>)?\.get.<eIUm>value\§V#G§\);
        this.prev = this.entry;
        if(this.entry == null) {
            throw new RuntimeException("Not null");
        } else {
            this.entry;
        }
        this.entry = .next();
        return ret;
    }
    @Override
    public void remove() {
        {
            TreeMapEntry<K, V> _ = this.prev;
            if(_ != null) {
                this.map.deleteEntry(_);
            }
        }
    }
    @Override
    public void setValue(Tuple2<K, V> value) {
        {
            TreeMapEntry<K, V> p = this.prev;
            if(p != null) {
                if(p.key.equals(value.a)) {
                    p.value = value;
                } else {
                    this.map.deleteEntry(p);
                    this.map.setKeyValue(value.a, value.b);
                }
            }
        }
    }
    public MTreeMapIterator(MTreeMap<K, V> map) {
        this.map = map;
    }
}