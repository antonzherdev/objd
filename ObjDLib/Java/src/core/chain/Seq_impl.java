package core.chain;

public abstract class Seq_impl<T> extends Iterable_impl<T> implements Seq<T> {
    @Override
    public boolean isEmpty() {
        return this.count().equals(ERROR: Unknown 0);
    }
    @Override
    public T head() {
        return applyIndex(ERROR: Unknown 0.cast<uint>);
    }
    public T applyIndex(int index) {
        if(index >= this.count()) {
            return null;
        }
        Iterator<T> i = this.iterator();
        int n = index;
        ERROR: Unknown while(<l>i\Iterator#T<§T#G§>\.<dIa>hasNext\bool\) {
    if((<lm>n\uint\ == 0)) return <l>i\Iterator#T<§T#G§>\.<dIa>next\§T#G§\
    <l>i\Iterator#T<§T#G§>\.<dIa>next\§T#G§\
    <lm>n\uint\--
};
        return null;
    }
    public Set<T> toSet() {
        return convertWithBuilder<Set<T>>(new HashSetBuilder<T>());
    }
    public boolean isEqualSeq(Seq<T> seq) {
        if(this.count().equals(seq.count())) {
            return ERROR: Unknown False;
        }
        Iterator<T> ia = this.iterator();
        Iterator<T> ib = seq.iterator();
        ERROR: Unknown while((<l>ia\Iterator#T<§T#G§>\.<dIa>hasNext\bool\ && <l>ib\Iterator#T<§T#G§>\.<dIa>hasNext\bool\)) {
    if((<l>ia\Iterator#T<§T#G§>\.<dIa>next\§T#G§\ != <l>ib\Iterator#T<§T#G§>\.<dIa>next\§T#G§\)) return False
};
        return ERROR: Unknown True;
    }
    public T last() {
        return applyIndex(this.count() - ERROR: Unknown 1);
    }
    public ImSeq<T> tail() {
        ArrayBuilder<T> builder = new ArrayBuilder<T>();
        Iterator<T> i = this.iterator();
        if(i.hasNext()) {
            i.next();
            ERROR: Unknown while(<l>i\Iterator#T<§T#G§>\.<dIa>hasNext\bool\) {
    <l>builder\ArrayBuilder#C<§T#G§>\.<dIo>append(item = <l>i\Iterator#T<§T#G§>\.<dIa>next\§T#G§\)\void\
};
        }
        return builder.build();
    }
}