package core.chain;

public interface ImSet<T> extends Set<T>, ImIterable<T> {
    MSet<T> mCopy();
}