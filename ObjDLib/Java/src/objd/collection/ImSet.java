package objd.collection;

public interface ImSet<T> extends Set<T>, ImIterable<T> {
    @Override
    MSet<T> mCopy();
}