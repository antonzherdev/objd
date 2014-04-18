package core.chain;

public interface ImIterable<T> extends Iterable<T>, ImTraversable<T> {
    @Override
    MIterable<T> mCopy();
}