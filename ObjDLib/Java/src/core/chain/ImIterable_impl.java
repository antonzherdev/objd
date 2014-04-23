package core.chain;

public abstract class ImIterable_impl<T> extends Iterable_impl<T> implements ImIterable<T> {
    @Override
    public MIterable<T> mCopy() {
        MArray<T> arr = new MArray<T>();
        forEach(new P<T>() {
            @Override
            public void apply(T item) {
                arr.appendItem(item);
            }
        });
        return arr;
    }
}