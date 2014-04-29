package objd.collection;

import objd.lang.*;

public final class EmptyList<T> extends ImList<T> {
    public static final EmptyList<Object> instance;
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
    public ImList<T> filterF(final F<T, Boolean> f) {
        return this;
    }
    @Override
    public ImList<T> reverse() {
        return this;
    }
    @Override
    public void forEach(final P<T> each) {
    }
    @Override
    public <C extends Comparable<C>> ImList<T> insertItem(final C item) {
        return ImList.<T>applyItem(((T)item));
    }
    public EmptyList() {
        this.instance = new EmptyList<Object>();
    }
}