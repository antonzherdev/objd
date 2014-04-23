package core.chain;

public abstract class MIterable_impl<T> extends Iterable_impl<T> implements MIterable<T> {
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
    @Override
    public ImIterable<T> im() {
        return this.imCopy();
    }
    @Override
    public ImIterable<T> imCopy() {
        MArray<T> arr = new MArray<T>();
        forEach(new P<T>() {
            @Override
            public void apply(T item) {
                arr.appendItem(item);
            }
        });
        return arr.im();
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