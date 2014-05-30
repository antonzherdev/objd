package objd.collection;

import objd.lang.*;

public abstract class ImSet_impl<T> extends Set_impl<T> implements ImSet<T> {
    public ImSet_impl() {
    }
    @Override
    public MSet<T> mCopy() {
        final MHashSet<T> arr = new MHashSet<T>();
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