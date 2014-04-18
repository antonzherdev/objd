package core.chain;

public class EmptyList<T> extends ImList<T> {
    public static final EmptyList<Object> instance = new EmptyList<Object>();
    @Override
    public int count() {
        return ERROR: Unknown 0.cast<uint>;
    }
    @Override
    public T head() {
        return null;
    }
    @Override
    public ImList<T> tail() {
        return this;
    }
    @Override
    public boolean isEmpty() {
        return ERROR: Unknown True;
    }
    @Override
    public ImList<T> filterF(F<T, Boolean> f) {
        return this;
    }
    @Override
    public ImList<T> reverse() {
        return this;
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