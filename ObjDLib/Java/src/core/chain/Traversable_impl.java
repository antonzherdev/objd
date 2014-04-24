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
                DispatchQueue.default.asyncF(new P0() {
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
        T ret = null;
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean apply(final T x) {
                if(where.apply(x)) {
                    ret = x;
                    return false;
                } else {
                    return true;
                }
            }
        });
        return ret;
    }
    public boolean existsWhere(final F<T, Boolean> where) {
        boolean ret = false;
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean apply(final T x) {
                if(where.apply(x)) {
                    ret = true;
                    return false;
                } else {
                    return true;
                }
            }
        });
        return ret;
    }
    public boolean allConfirm(final F<T, Boolean> confirm) {
        boolean ret = true;
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean apply(final T x) {
                if(!(confirm.apply(x))) {
                    ret = false;
                    return false;
                } else {
                    return true;
                }
            }
        });
        return ret;
    }
    public T head() {
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean apply(final T on) {
                ret = on;
                return false;
            }
        });
        return ret;
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