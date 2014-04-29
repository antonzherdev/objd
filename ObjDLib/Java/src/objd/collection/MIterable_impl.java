package objd.collection;

import objd.lang.*;

public abstract class MIterable_impl<T> extends Iterable_impl<T> implements MIterable<T> {
    @Override
    public boolean removeItem(final T item) {
        final MIterator<T> i = this.mutableIterator();
        boolean ret = false;
        while(i.hasNext()) {
            if(i.next().equals(item)) {
                i.remove();
                ret = true;
            }
        }
        return ret;
    }
    @Override
    public ImIterable<T> im() {
        return this.imCopy();
    }
    @Override
    public ImIterable<T> imCopy() {
        final MArray<T> arr = new MArray<T>();
        {
            final Iterator<T> __inline__1_i = this.iterator();
            while(__inline__1_i.hasNext()) {
                final T item = __inline__1_i.next();
                arr.appendItem(item);
            }
        }
        return arr.im();
    }
    public void mutableFilterBy(final F<T, Boolean> by) {
        final MIterator<T> i = this.mutableIterator();
        while(i.hasNext()) {
            if(by.apply(i.next())) {
                i.remove();
            }
        }
    }
}