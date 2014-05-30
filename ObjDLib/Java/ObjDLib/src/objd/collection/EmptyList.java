package objd.collection;

import objd.lang.*;

public final class EmptyList<T> extends ImList<T> {
    public static final EmptyList<Object> instance;
    @Override
    public int count() {
        return ((int)(0));
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
    public <C extends Comparable<C>> ImList<C> insertItem(final C item) {
        return ImList.<C>applyItem(item);
    }
    public EmptyList() {
    }
    public String toString() {
        return "EmptyList";
    }
    public boolean equals(final Object to) {
        if(this == to) {
            return true;
        }
        if(to == null || !(to instanceof EmptyList)) {
            return false;
        }
        return true;
    }
    public int hashCode() {
        return 0;
    }
    static {
        instance = new EmptyList<Object>();
    }
}