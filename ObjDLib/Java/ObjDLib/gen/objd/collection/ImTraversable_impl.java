package objd.collection;

import objd.lang.*;

public abstract class ImTraversable_impl<T> extends Traversable_impl<T> implements ImTraversable<T> {
    public ImTraversable_impl() {
    }
    public MTraversable<T> mCopy() {
        final MArray<T> arr = new MArray<T>();
        forEach(new P<T>() {
            @Override
            public void apply(final T item) {
                arr.appendItem(item);
            }
        });
        return arr;
    }
}