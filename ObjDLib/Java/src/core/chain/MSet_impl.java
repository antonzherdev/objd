package core.chain;

public abstract class MSet_impl<T> extends Set_impl<T> implements MSet<T> {
    @Override
    public ImSet<T> im() {
        return this.imCopy();
    }
    @Override
    public ImSet<T> imCopy() {
        MHashSet<T> arr = new MHashSet<T>();
        {
            Iterator<T> __inline__1_i = this.iterator();
            while(__inline__1_i.hasNext()) {
                T item = __inline__1_i.next();
                arr.appendItem(item);
            }
        }
        return arr.im();
    }
    @Override
    public boolean removeItem(T item) {
        MIterator<T> i = this.mutableIterator();
        boolean ret = false;
        while(i.hasNext()) {
            if(i.next().equals(item)) {
                i.remove();
                ret = true;
            }
        }
        return ret;
    }
    public void mutableFilterBy(F<T, Boolean> by) {
        MIterator<T> i = this.mutableIterator();
        while(i.hasNext()) {
            if(by.apply(i.next())) {
                i.remove();
            }
        }
    }
}