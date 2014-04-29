package objd.collection;

public interface ImTraversable<T> extends Traversable<T> {
    MTraversable<T> mCopy();
}