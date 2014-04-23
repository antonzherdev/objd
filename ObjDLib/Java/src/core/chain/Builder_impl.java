package core.chain;

public abstract class Builder_impl<T, C extends Traversable<T>> implements Builder<T, C> {
    public void appendAllItems(Traversable<T> items) {
        items.forEach(new P<T>() {
            @Override
            public void apply(T _) {
                appendItem(_);
            }
        });
    }
}