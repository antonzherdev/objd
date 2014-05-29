package objd.collection;

import objd.lang.*;
import objd.concurrent.DispatchQueue;

public abstract class Iterable_impl<T> extends Traversable_impl<T> implements Iterable<T> {
    public Iterable_impl() {
    }
    @Override
    public T head() {
        if(this.isEmpty()) {
            return null;
        } else {
            return ((T)(this.iterator().next()));
        }
    }
    @Override
    public void forEach(final P<T> each) {
        final Iterator<T> i = this.iterator();
        while(i.hasNext()) {
            each.apply(i.next());
        }
    }
    @Override
    public void parForEach(final P<T> each) {
        final Iterator<T> i = this.iterator();
        while(i.hasNext()) {
            final T v = i.next();
            DispatchQueue.aDefault.asyncF(new P0() {
                @Override
                public void apply() {
                    each.apply(v);
                }
            });
        }
    }
    @Override
    public Go goOn(final F<T, Go> on) {
        Go ret = Go.Continue;
        final Iterator<T> i = this.iterator();
        while(i.hasNext()) {
            if(on.apply(i.next()) == Go.Break) {
                ret = Go.Break;
                break;
            }
        }
        return ret;
    }
    @Override
    public boolean containsItem(final T item) {
        final Iterator<T> i = this.iterator();
        while(i.hasNext()) {
            if(i.next().equals(i)) {
                return true;
            }
        }
        return false;
    }
    @Override
    public String toString() {
        return this.chain().toStringStartDelimiterEnd("[", ", ", "]");
    }
    @Override
    public int hashCode() {
        int ret = ((int)(13));
        final Iterator<T> i = this.iterator();
        while(i.hasNext()) {
            ret = ret * 31 + i.next().hashCode();
        }
        return ret;
    }
    public int count() {
        final Iterator<T> i = this.iterator();
        int n = ((int)(0));
        while(i.hasNext()) {
            i.next();
            n++;
        }
        return n;
    }
    public boolean isEmpty() {
        return !(this.iterator().hasNext());
    }
    public boolean isEqualIterable(final Iterable<T> iterable) {
        if(this.count() == iterable.count()) {
            return true;
        } else {
            final Iterator<T> ai = this.iterator();
            final Iterator<T> bi = iterable.iterator();
            while(ai.hasNext() && bi.hasNext()) {
                if(!(ai.next().equals(bi.next()))) {
                    return false;
                }
            }
            return true;
        }
    }
    public boolean equals(final Object to) {
        if(this == to) {
            return true;
        }
        if(to == null) {
            return false;
        }
        if(to instanceof Iterable) {
            return isEqualIterable(((Iterable<T>)(((Iterable)(to)))));
        }
        return false;
    }
}