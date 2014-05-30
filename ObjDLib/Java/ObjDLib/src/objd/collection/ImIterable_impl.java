package objd.collection;

public abstract class ImIterable_impl<T> extends Iterable_impl<T> implements ImIterable<T> {
    public ImIterable_impl() {
    }
    @Override
    public MIterable<T> mCopy() {
        final MArray<T> arr = new MArray<T>();
        {
            final Iterator<T> __il__1i = this.iterator();
            while(__il__1i.hasNext()) {
                final T item = __il__1i.next();
                arr.appendItem(item);
            }
        }
        return arr;
    }
}