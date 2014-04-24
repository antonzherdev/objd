package core.chain;

public abstract class MSeq_impl<T> extends Seq_impl<T> implements MSeq<T> {
    @Override
    public ImSeq<T> im() {
        return this.imCopy();
    }
    @Override
    public ImSeq<T> imCopy() {
        MArray<T> arr = new MArray<T>();
        {
            Iterator<T> __inline__1_i = this.iterator();
            while(__inline__1_i.hasNext()) {
                T item = __inline__1_i.next();
                arr.appendItem(item);
            }
        }
        return arr.im();
    }
    public boolean removeIndex(int index) {
        MIterator<T> i = this.mutableIterator();
        int j = index;
        boolean ret = false;
        while(i.hasNext()) {
            i.next();
            if(j.equals(0)) {
                i.remove();
                ret = true;
                break;
            }
            j--;
        }
        return ret;
    }
    public void setIndexItem(int index,T item) {
        MIterator<T> i = this.mutableIterator();
        int n = index;
        while(i.hasNext()) {
            if(n.equals(0)) {
                i.next();
                i.setValue(item);
                return ;
            }
            i.next();
            n--;
        }
        throw new RuntimeException("Incorrect index");
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