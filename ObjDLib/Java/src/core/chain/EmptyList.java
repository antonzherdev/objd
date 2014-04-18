package core.chain;

public class EmptyList<T> extends ImList<T> {
    public static final EmptyList<Object> instance = new EmptyList();
    public int count() {
        return ERROR: Unknown 0.cast<uint>;
    }
    public T head() {
        return ERROR: Unknown none<T#G>;
    }
    public ImList<T> tail() {
        return ERROR: Unknown <EmptyList#C<T#G>>self;
    }
    public boolean isEmpty() {
        return ERROR: Unknown True;
    }
    public ImList<T> filterF(F<T, Boolean> f) {
        return ERROR: Unknown <EmptyList#C<T#G>>self;
    }
    public ImList<T> reverse() {
        return ERROR: Unknown <EmptyList#C<T#G>>self;
    }
    public void forEach(F<T, Void> each) {
    }
    public ImList<T> insertItem(C item) {
        return ERROR: Unknown <to>ImList\ImList#C.class\.<dIt>apply(item = <l>item\C#G\.cast<T#G>)\ImList#C<§T#G§>\;
    }
    public EmptyList() {
    }
    static final ClassType<EmptyList<T>> type;
}