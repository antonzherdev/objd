package core.chain;

public class EmptyList<T> extends ImList<T> {
    public static final EmptyList<Object> instance = new EmptyList<Object>();
    @Override
    public int count() {
        return ERROR: Unknown 0.cast<uint>;
    }
    @Override
    public T head() {
        return ERROR: Unknown none<T#G>;
    }
    @Override
    public ImList<T> tail() {
        return ERROR: Unknown <EmptyList#C<T#G>>self;
    }
    @Override
    public boolean isEmpty() {
        return ERROR: Unknown True;
    }
    @Override
    public ImList<T> filterF(F<T, Boolean> f) {
        return ERROR: Unknown <EmptyList#C<T#G>>self;
    }
    @Override
    public ImList<T> reverse() {
        return ERROR: Unknown <EmptyList#C<T#G>>self;
    }
    @Override
    public void forEach(P<T> each) {
    }
    @Override
    public ImList<T> insertItem(C item) {
        return ImList().applyItem<T>(item.ERROR: Unknown cast<T#G>);
    }
    public EmptyList() {
    }
}