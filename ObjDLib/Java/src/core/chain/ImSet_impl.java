package core.chain;

public abstract class ImSet_impl<T> extends Set_impl<T> implements ImSet<T> {
    @Override
    public MSet<T> mCopy() {
        MHashSet<T> arr = new MHashSet<T>();
        forEach(new P<T>() {
            @Override
            public void apply(T item) {
                arr.appendItem(item);
            }
        });
        return arr;
    }
}