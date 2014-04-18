package core.chain;

public class TreeSetBuilder<T> implements Builder<T, ImTreeSet<T>> {
    public final F2<T, T, Integer> comparator;
    private final MTreeSet<T> set = ERROR: Unknown <to>MTreeSet\MTreeSet#C.class\.<dIt>apply(comparator = <TreeSetBuilder#C<T#G>>self.<eIU>comparator\(§T#G§, §T#G§) -> int\)\MTreeSet#C<§T#G§>\;
    public static TreeSetBuilder<T> apply() {
        return new TreeSetBuilder(ERROR: Unknown a : T#G, b : T#G -> int = weak return <l>a\T#G\.<rdI>compare(to = <l>b\T#G\)\int\);
    }
    public void appendItem(T item) {
        ERROR: Unknown <TreeSetBuilder#C<T#G>>self.<ep>set\MTreeSet#C<§T#G§>\.<dIo>append(item = <l>item\§T#G§\)\void\;
    }
    public ImTreeSet<T> build() {
        return ERROR: Unknown <TreeSetBuilder#C<T#G>>self.<ep>set\MTreeSet#C<§T#G§>\.<dIo>im\ImTreeSet#C<§T#G§>\;
    }
    public TreeSetBuilder(F2<T, T, Integer> comparator) {
    }
    static final ClassType<TreeSetBuilder<T>> type;
    public void appendAllItems(Traversable<T> items) {
        ERROR: Unknown <l>items\Traversable#T<§T#G§>\.<dI>for(each = _ : §T#G§ -> void = <Builder#T<T#G, C#G>>self.<dIa>append(item = <l>_\§T#G§\)\void\)\void\;
    }
}