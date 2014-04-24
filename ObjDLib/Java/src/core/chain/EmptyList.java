package core.chain;

public final class EmptyList<T> extends ImList<T> {
    public static final EmptyList<Object> instance = new EmptyList<Object>();
    @Override
    public int count() {
        return ((int)0);
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
        return true;
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
    public  <C extends Comparable<C>> ImList<T> insertItem(C item) {
        return ImList().applyItem<T>(((T)item));
    }
    public EmptyList() {
    }
}