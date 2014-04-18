package core.chain;

public class HashSetBuilder<T> implements Builder<T, ImHashSet<T>> {
    public final MHashSet<T> set = new MHashSet();
    public void appendItem(T item) {
        ERROR: Unknown <HashSetBuilder#C<T#G>>self.<eI>set\MHashSet#C<§T#G§>\.<rdIa>append(item = <l>item\§T#G§\)\void\;
    }
    public ImHashSet<T> build() {
        return ERROR: Unknown <HashSetBuilder#C<T#G>>self.<eI>set\MHashSet#C<§T#G§>\.<dIo>im\ImHashSet#C<§T#G§>\;
    }
    public HashSetBuilder() {
    }
    static final ClassType<HashSetBuilder<T>> type;
    public void appendAllItems(Traversable<T> items) {
        ERROR: Unknown <l>items\Traversable#T<§T#G§>\.<dI>for(each = _ : §T#G§ -> void = <Builder#T<T#G, C#G>>self.<dIa>append(item = <l>_\§T#G§\)\void\)\void\;
    }
}