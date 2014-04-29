package objd.collection;

public interface Seq<T> extends Iterable<T> {
    T applyIndex(final int index);
    Set<T> toSet();
    boolean isEqualSeq(final Seq<T> seq);
    @Override
    boolean isEmpty();
    @Override
    T head();
    T last();
    ImSeq<T> tail();
}