package core.chain;

public interface MSeq<T> extends Seq<T>, MIterable<T> {
    boolean removeIndex(int index);
    void insertIndexItem(int index,T item);
    void prependItem(T item);
    void setIndexItem(int index,T item);
    @Override
    ImSeq<T> im();
    @Override
    ImSeq<T> imCopy();
}