package objd.collection;

import objd.lang.*;

public abstract class Builder_impl<T, C extends Traversable<T>> implements Builder<T, C> {
    public void appendAllItems(final Traversable<T> items) {
        if(items instanceof Iterable) {
            {
                final Iterator<T> __il__0t_0i = ((Iterable<T>)(((Iterable)(items)))).iterator();
                while(__il__0t_0i.hasNext()) {
                    final T _ = __il__0t_0i.next();
                    appendItem(_);
                }
            }
        } else {
            items.forEach(new P<T>() {
                @Override
                public void apply(final T _) {
                    appendItem(_);
                }
            });
        }
    }
}