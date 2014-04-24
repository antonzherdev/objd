package core.chain;

public abstract class Traversable_impl<T> implements Traversable<T> {
    public void forEach(P<T> each) {
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean apply(T item) {
                each.apply(item);
                return true;
            }
        });
    }
    public void parForEach(P<T> each) {
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean apply(T item) {
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
    public T findWhere(F<T, Boolean> where) {
        T ret = null;
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean apply(T x) {
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
    public boolean existsWhere(F<T, Boolean> where) {
        boolean ret = false;
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean apply(T x) {
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
    public boolean allConfirm(F<T, Boolean> confirm) {
        boolean ret = true;
        goOn(new F<T, Boolean>() {
            @Override
            public Boolean apply(T x) {
                if(ERROR: Unknown !(<l>confirm\§T#G§ -> bool\.<d>apply( = <l>x\§T#G§\)\bool\)) {
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
            public Boolean apply(T on) {
                ret = on;
                return false;
            }
        });
        return ret;
    }
    public  <C extends Traversable<T>> C convertWithBuilder(Builder<T, C> builder) {
        forEach(new P<T>() {
            @Override
            public void apply(T x) {
                builder.appendItem(x);
            }
        });
        return builder.build();
    }
}