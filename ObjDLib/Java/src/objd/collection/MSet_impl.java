package objd.collection;

import objd.lang.*;

public abstract class MSet_impl<T> extends Set_impl<T> implements MSet<T> {
    public MSet_impl() {
    }
    @Override
    public ImSet<T> im() {
        return this.imCopy();
    }
    @Override
    public ImSet<T> imCopy() {
        final MHashSet<T> arr = new MHashSet<T>();
        {
            final Iterator<T> __il__1i = this.iterator();
            while(__il__1i.hasNext()) {
                final T item = __il__1i.next();
                arr.appendItem(item);
            }
        }
        return arr.im();
    }
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
    public void mutableFilterBy(final F<T, Boolean> by) {
        final MIterator<T> i = this.mutableIterator();
        while(i.hasNext()) {
            if(by.apply(i.next())) {
                i.remove();
            }
        }
    }
}