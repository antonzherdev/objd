package core.chain;

public class MTreeMapKeyIterator<K> implements MIterator<K> {
    public final MTreeMap<K, ?> map;
    private TreeMapEntry<K, ?> prev;
    public TreeMapEntry<K, ?> entry;
    public static MTreeMapKeyIterator<K> applyMapEntry(MTreeMap<K, ?> map,TreeMapEntry<K, ?> entry) {
        ERROR: Unknown local ret : MTreeMapKeyIterator#C<§K#G§> = <to>MTreeMapKeyIterator\MTreeMapKeyIterator#C.class\.<tcI>apply(map = <l>map\MTreeMap#C<K#G, ^_>\)\MTreeMapKeyIterator#C<§K#G§>\;
        ret.entry = entry;
        return ret;
    }
    @Override
    public boolean hasNext() {
        return this.entry != null;
    }
    @Override
    public K next() {
        ERROR: Unknown local ret : K#G = <MTreeMapKeyIterator#C<K#G>>self.<eIm>entry\(^TreeMapEntry#C<§K#G§, ^_>)?\.get.<eIUm>key\§K#G§\;
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
            ERROR: Unknown local _ : ^(^TreeMapEntry#C<§K#G§, ^_>)¿ = <MTreeMapKeyIterator#C<K#G>>self.<emp>prev\(^TreeMapEntry#C<§K#G§, ^_>)?\;
            if(_ != null) {
                this.map.deleteEntry(_);
            }
        }
    }
    @Override
    public void setValue(K value) {
        {
            ERROR: Unknown local p : ^(^TreeMapEntry#C<§K#G§, ^_>)¿ = <MTreeMapKeyIterator#C<K#G>>self.<emp>prev\(^TreeMapEntry#C<§K#G§, ^_>)?\;
            if(p != null) {
                if(p.key.equals(value)) {
                    this.map.deleteEntry(p);
                    this.map.setKeyValue(value, p.value);
                }
            }
        }
    }
    public MTreeMapKeyIterator(MTreeMap<K, ?> map) {
    }
}