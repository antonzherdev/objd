package core.chain;

public abstract class MSet_impl<T> extends Set_impl<T> implements MSet<T> {
    @Override
    public ImSet<T> im() {
        return this.imCopy();
    }
    @Override
    public ImSet<T> imCopy() {
        MHashSet<T> arr = new MHashSet<T>();
        forEach(new P<T>() {
            @Override
            public void apply(T item) {
                arr.appendItem(item);
            }
        });
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