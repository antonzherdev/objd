package core.chain;

public abstract class ImIterable_impl<T> extends Iterable_impl<T> implements ImIterable<T> {
    @Override
    public MIterable<T> mCopy() {
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
}