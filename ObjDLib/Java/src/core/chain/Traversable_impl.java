package core.chain;

public abstract class Traversable_impl<T> implements Traversable<T> {
    public void forEach(final P<T> each) {
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean apply(final T item) {
                each.apply(item);
                return true;
            }
        });
    }
    public void parForEach(final P<T> each) {
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean apply(final T item) {
                DispatchQueue.aDefault.asyncF(new P0() {
                    @Override
                    public void apply() {
                        each.apply(item);
                    }
                });
                return true;
            }
        });
    }
    public Chain<T> chain() {
        return Chain.<T>chainWithCollection(this);
    }
    public T findWhere(final F<T, Boolean> where) {
        final Mut<T> ret = new Mut<T>(null);
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean apply(final T x) {
                if(where.apply(x)) {
                    ret.value = x;
                    return false;
                } else {
                    return true;
                }
            }
        });
        return ret.value;
    }
    public boolean existsWhere(final F<T, Boolean> where) {
        final Mut<boolean> ret = new Mut<boolean>(false);
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean apply(final T x) {
                if(where.apply(x)) {
                    ret.value = true;
                    return false;
                } else {
                    return true;
                }
            }
        });
        return ret.value;
    }
    public boolean allConfirm(final F<T, Boolean> confirm) {
        final Mut<boolean> ret = new Mut<boolean>(true);
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean apply(final T x) {
                if(!(confirm.apply(x))) {
                    ret.value = false;
                    return false;
                } else {
                    return true;
                }
            }
        });
        return ret.value;
    }
    public T head() {
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean apply(final T on) {
                ret.value = on;
                return false;
            }
        });
        return ret.value;
    }
    public <C extends Traversable<T>> C convertWithBuilder(final Builder<T, C> builder) {
        forEach(new P<T>() {
            @Override
            public void apply(final T x) {
                builder.appendItem(x);
            }
        });
        return builder.build();
    }
}