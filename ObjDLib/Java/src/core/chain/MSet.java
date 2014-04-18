package core.chain;

public interface MSet<T> extends Set<T>, MIterable<T> {
    ImSet<T> im();
    ImSet<T> imCopy();
}