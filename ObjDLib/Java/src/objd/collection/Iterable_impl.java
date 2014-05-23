package objd.collection;

import objd.lang.*;
import objd.concurrent.DispatchQueue;

public abstract class Iterable_impl<T> extends Traversable_impl<T> implements Iterable<T> {
    @Override
    public T head() {
        if(this.isEmpty()) {
            return null;
        } else {
            return this.iterator().next();
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
            final Go b = on.apply(i.next());
            if(!(b)) {
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
}