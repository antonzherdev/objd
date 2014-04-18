package core.chain;

public interface Seq<T> extends Iterable<T> {
    T applyIndex(int index);
    Set<T> toSet();
    boolean isEqualSeq(Seq<T> seq);
    boolean isEmpty();
    T head();
    T last();
    ImSeq<T> tail();
}