package objd.collection;

import objd.lang.*;

public abstract class MTraversable_impl<T> extends Traversable_impl<T> implements MTraversable<T> {
    public ImTraversable<T> im() {
        return this.imCopy();
    }
    public ImTraversable<T> imCopy() {
        final MArray<T> arr = new MArray<T>();
        forEach(new P<T>() {
            @Override
            public void apply(final T item) {
                arr.appendItem(item);
            }
        });
        return arr.im();
    }
}