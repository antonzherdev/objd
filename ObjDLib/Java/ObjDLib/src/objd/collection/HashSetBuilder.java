package objd.collection;

public class HashSetBuilder<T> extends Builder_impl<T, ImHashSet<T>> {
    public final MHashSet<T> set;
    @Override
    public void appendItem(final T item) {
        this.set.appendItem(item);
    }
    @Override
    public ImHashSet<T> build() {
        return this.set.im();
    }
    public HashSetBuilder(final int capacity) {
        this.set = new MHashSet<T>(capacity);
    }
    static public <T> HashSetBuilder<T> apply() {
        return new HashSetBuilder<T>(((int)(0)));
    }
    public String toString() {
        return "HashSetBuilder";
    }
}