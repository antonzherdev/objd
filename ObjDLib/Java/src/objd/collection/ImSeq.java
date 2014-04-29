package objd.collection;

import objd.lang.*;

public interface ImSeq<T> extends Seq<T>, ImIterable<T> {
    ImSeq<T> addItem(final T item);
    ImSeq<T> addSeq(final Seq<T> seq);
    ImSeq<T> subItem(final T item);
    @Override
    MSeq<T> mCopy();
}