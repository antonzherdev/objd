package objd.collection;

public interface MSet<T> extends Set<T>, MIterable<T> {
    @Override
    ImSet<T> im();
    @Override
    ImSet<T> imCopy();
    String toString();
}