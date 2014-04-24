package core.chain;

public class HashSetBuilder<T> extends Builder_impl<T, ImHashSet<T>> {
    public final MHashSet<T> set;
    @Override
    public void appendItem(T item) {
        this.set.appendItem(item);
    }
    @Override
    public ImHashSet<T> build() {
        return this.set.im();
    }
    public HashSetBuilder() {
        this.set = new MHashSet<T>();
    }
}