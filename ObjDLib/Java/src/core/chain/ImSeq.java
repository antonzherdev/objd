package core.chain;

public interface ImSeq<T> extends Seq<T>, ImIterable<T> {
    ImSeq<T> addItem(T item);
    ImSeq<T> addSeq(Seq<T> seq);
    ImSeq<T> subItem(T item);
    MSeq<T> mCopy();
}