package core.chain;

public abstract class ImSeq_impl<T> extends Seq_impl<T> implements ImSeq<T> {
    @Override
    public MSeq<T> mCopy() {
        MArray<T> arr = new MArray<T>();
        {
            Iterator<T> __inline__1_i = this.iterator();
            while(__inline__1_i.hasNext()) {
                T item = __inline__1_i.next();
                arr.appendItem(item);
            }
        }
        return arr;
    }
    public ImSeq<T> addItem(T item) {
        ArrayBuilder<T> builder = new ArrayBuilder<T>();
        builder.appendAllItems(this);
        builder.appendItem(item);
        return builder.build();
    }
    public ImSeq<T> addSeq(Seq<T> seq) {
        ArrayBuilder<T> builder = new ArrayBuilder<T>();
        builder.appendAllItems(this);
        builder.appendAllItems(seq);
        return builder.build();
    }
    public ImSeq<T> subItem(T item) {
        return this.chain().filter(new F<T, Boolean>() {
            @Override
            public Boolean apply(T _) {
                return _.equals(item);
            }
        }).toArray();
    }
}