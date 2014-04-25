package core.chain;

public abstract class ImSeq_impl<T> extends Seq_impl<T> implements ImSeq<T> {
    @Override
    public MSeq<T> mCopy() {
        final MArray<T> arr = new MArray<T>();
        {
            final Iterator<T> __inline__1_i = this.iterator();
            while(__inline__1_i.hasNext()) {
                final T item = __inline__1_i.next();
                arr.appendItem(item);
            }
        }
        return arr;
    }
    public ImSeq<T> addItem(final T item) {
        final ArrayBuilder<T> builder = new ArrayBuilder<T>();
        builder.appendAllItems(this);
        builder.appendItem(item);
        return builder.build();
    }
    public ImSeq<T> addSeq(final Seq<T> seq) {
        final ArrayBuilder<T> builder = new ArrayBuilder<T>();
        builder.appendAllItems(this);
        builder.appendAllItems(seq);
        return builder.build();
    }
    public ImSeq<T> subItem(final T item) {
        return this.chain().filter(new F<T, Boolean>() {
            @Override
            public Boolean apply(final T _) {
                return !(_.equals(item));
            }
        }).toArray();
    }
}