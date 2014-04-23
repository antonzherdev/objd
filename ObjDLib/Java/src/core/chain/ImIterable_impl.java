package core.chain;

public abstract class ImIterable_impl<T> extends Iterable_impl<T> implements ImIterable<T> {
    @Override
    public MIterable<T> mCopy() {
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
}