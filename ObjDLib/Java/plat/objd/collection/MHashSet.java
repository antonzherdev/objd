package objd.collection;

import objd.lang.F;

public class MHashSet<T> extends HashSet<T> implements MSet<T> {
    public MHashSet() {
        super(new java.util.HashSet<T>());
    }

    public MHashSet(java.util.HashSet<T> set) {
        super(set);
    }


    @Override
    public MIterator<T> mutableIterator() {
        final java.util.Iterator<T> i = set.iterator();
        return new MIterator<T>() {
            @Override
            public void remove() {
                i.remove();
            }

            @Override
            public void setValue(T value) {
                i.remove();
                set.add(value);
            }

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
    public void appendItem(T item) {
        set.add(item);
    }

    @Override
    public boolean removeItem(T item) {
        return set.remove(item);
    }

    @Override
    public void clear() {
        set.clear();
    }

    @Override
    public void mutableFilterBy(F<T, Boolean> by) {
        java.util.Iterator<T> i = set.iterator();
        while(i.hasNext()) {
            if(by.apply(i.next())) {
                i.remove();
            }
        }
    }

    @Override
    public ImHashSet<T> im() {
        return new ImHashSet<T>(set);
    }

    @Override
    public ImHashSet<T> imCopy() {
        return new ImHashSet<T>(new java.util.HashSet<T>(set));
    }
}
