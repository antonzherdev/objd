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
        boolean ret = ERROR: Unknown False;
        ERROR: Unknown while(<l>i\MIterator#T<§T#G§>\.<rdIa>hasNext\bool\) {
    if((<l>i\MIterator#T<§T#G§>\.<rdIa>next\§T#G§\ == <l>item\§T#G§\)) {
    <l>i\MIterator#T<§T#G§>\.<dIa>remove\void\
    (<lm>ret\bool\ = True)
}
};
        return ret;
    }
    public void mutableFilterBy(F<T, Boolean> by) {
        MIterator<T> i = this.mutableIterator();
        ERROR: Unknown while(<l>i\MIterator#T<§T#G§>\.<rdIa>hasNext\bool\) {
    if(<l>by\§T#G§ -> bool\.<d>apply( = <l>i\MIterator#T<§T#G§>\.<rdIa>next\§T#G§\)\bool\) <l>i\MIterator#T<§T#G§>\.<dIa>remove\void\
};
    }
}