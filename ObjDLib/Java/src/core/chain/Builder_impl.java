package core.chain;

public abstract class Builder_impl<T, C extends Traversable<T>> implements Builder<T, C> {
    public void appendAllItems(Traversable<T> items) {
        if(items instanceof Iterable) {
            {
                Iterator<T> __inline__0_0_i = ((Iterable<T>)items).iterator();
                while(__inline__0_0_i.hasNext()) {
                    T _ = __inline__0_0_i.next();
                    appendItem(_);
                }
            }
        } else {
            items.forEach(new P<T>() {
                @Override
                public void apply(T _) {
                    appendItem(_);
                }
            });
        }
    }
}