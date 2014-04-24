package core.chain;

public abstract class ImSet_impl<T> extends Set_impl<T> implements ImSet<T> {
    @Override
    public MSet<T> mCopy() {
        final MHashSet<T> arr = new MHashSet<T>();
        {
            final Iterator<T> __inline__1_i = this.iterator();
            while(__inline__1_i.hasNext()) {
                final T item = __inline__1_i.next();
                arr.appendItem(item);
            }
        }
        return arr;
    }
}