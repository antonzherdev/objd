package objd.collection;

import objd.lang.*;

public interface MSeq<T> extends Seq<T>, MIterable<T> {
    boolean removeIndex(final int index);
    void insertIndexItem(final int index, final T item);
    void prependItem(final T item);
    void setIndexItem(final int index, final T item);
    @Override
    ImSeq<T> im();
    @Override
    ImSeq<T> imCopy();
}