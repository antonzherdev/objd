package objd.collection;

import objd.lang.F;
import objd.lang.P;

public abstract class HashSet<T> extends Set_impl<T> {
    protected final java.util.HashSet<T> set;

    public HashSet(java.util.HashSet<T> set) {
        this.set = set;
    }

    @Override
    public int count() {
        return set.size();
    }

    @Override
    public boolean isEmpty() {
        return set.isEmpty();
    }

    @Override
    public int hashCode() {
        return set.hashCode();
    }

    @Override
    public boolean containsItem(T item) {
        return set.contains(item);
    }

    @Override
    public void forEach(P<T> each) {
        for (T t : set) {
            each.apply(t);
        }
    }

    @Override
    public Go goOn(F<T, Go> on) {
        for (T t : set) {
            if(on.apply(t) == Go.Break) return Go.Break;
        }
        return Go.Continue;
    }

    @Override
    public Iterator<T> iterator() {
        final java.util.Iterator<T> i = set.iterator();
        return new Iterator<T>() {
            @Override
            public boolean hasNext() {
                return i.hasNext();
            }

            @Override
            public T next() {
                return i.next();
            }
        };
    }

    @Override
    public T findWhere(F<T, Boolean> where) {
        for (T t : set) {
            if(where.apply(t)) return t;
        }
        return null;
    }

    @Override
    public boolean existsWhere(F<T, Boolean> where) {
        for (T t : set) {
            if(where.apply(t)) return true;
        }
        return false;
    }

    @Override
    public boolean allConfirm(F<T, Boolean> confirm) {
        for (T t : set) {
            if(!confirm.apply(t)) return false;
        }
        return true;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        HashSet a = (HashSet) o;

        if (!set.equals(a.set)) return false;

        return true;
    }
}
