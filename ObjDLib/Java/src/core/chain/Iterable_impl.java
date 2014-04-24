package core.chain;

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
            DispatchQueue.default.asyncF(new P0() {
                @Override
                public void apply() {
                    each.apply(v);
                }
            });
        }
    }
    @Override
    public boolean goOn(final F<T, Boolean> on) {
        final Iterator<T> i = this.iterator();
        while(i.hasNext()) {
            if(!(on.apply(i.next()))) {
                return false;
            }
        }
        return true;
    }
    @Override
    public String toString() {
        return this.chain().toStringWithStartDelimiterEnd("[", ", ", "]");
    }
    @Override
    public int hashCode() {
        int ret = ((int)13);
        final Iterator<T> i = this.iterator();
        while(i.hasNext()) {
            ret = ret * 31 + i.next().hashCode();
        }
        return ret;
    }
    public int count() {
        final Iterator<T> i = this.iterator();
        int n = ((int)0);
        while(i.hasNext()) {
            i.next();
            n++;
        }
        return n;
    }
    public boolean isEmpty() {
        return !(this.iterator().hasNext());
    }
    public boolean containsItem(final T item) {
        final Iterator<T> i = this.iterator();
        while(i.hasNext()) {
            if(i.next().equals(i)) {
                return true;
            }
        }
        return false;
    }
}