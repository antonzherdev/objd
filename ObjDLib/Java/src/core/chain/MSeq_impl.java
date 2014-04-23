package core.chain;

public abstract class MSeq_impl<T> extends Seq_impl<T> implements MSeq<T> {
    @Override
    public ImSeq<T> im() {
        return this.imCopy();
    }
    @Override
    public ImSeq<T> imCopy() {
        MArray<T> arr = new MArray<T>();
        forEach(new P<T>() {
            @Override
            public void apply(T item) {
                arr.appendItem(item);
            }
        });
        return arr.im();
    }
    public boolean removeIndex(int index) {
        MIterator<T> i = this.mutableIterator();
        int j = index;
        boolean ret = ERROR: Unknown False;
        ERROR: Unknown while(<l>i\MIterator#T<§T#G§>\.<rdIa>hasNext\bool\) {
    <l>i\MIterator#T<§T#G§>\.<rdIa>next\§T#G§\
    if((<lm>j\uint\ == 0)) {
    <l>i\MIterator#T<§T#G§>\.<dIa>remove\void\
    (<lm>ret\bool\ = True)
    break
}
    <lm>j\uint\--
};
        return ret;
    }
    public void setIndexItem(int index,T item) {
        MIterator<T> i = this.mutableIterator();
        int n = index;
        ERROR: Unknown while(<l>i\MIterator#T<§T#G§>\.<rdIa>hasNext\bool\) {
    if((<lm>n\uint\ == 0)) {
    <l>i\MIterator#T<§T#G§>\.<rdIa>next\§T#G§\
    <l>i\MIterator#T<§T#G§>\.<dIa>set(value = <l>item\§T#G§\)\void\
    return nil
}
    <l>i\MIterator#T<§T#G§>\.<rdIa>next\§T#G§\
    <lm>n\uint\--
};
        throw new RuntimeException("Incorrect index");
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