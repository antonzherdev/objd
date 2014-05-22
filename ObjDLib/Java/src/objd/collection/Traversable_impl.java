package objd.collection;

import objd.lang.*;
import objd.concurrent.DispatchQueue;
import objd.chain.Chain;

public abstract class Traversable_impl<T> implements Traversable<T> {
    public void forEach(final P<T> each) {
        goOn(new F<T, Go>() {
            @Override
            public Go apply(final T item) {
                each.apply(item);
                return Go.Continue;
            }
        });
    }
    public void parForEach(final P<T> each) {
        goOn(new F<T, Go>() {
            @Override
            public Go apply(final T item) {
                DispatchQueue.aDefault.asyncF(new P0() {
                    @Override
                    public void apply() {
                        each.apply(item);
                    }
                });
                return Go.Continue;
            }
        });
    }
    public Chain<T> chain() {
        return Chain.<T>applyCollection(this);
    }
    public boolean containsItem(final T item) {
        final Mut<Boolean> ret = new Mut<Boolean>(false);
        goOn(new F<T, Go>() {
            @Override
            public Go apply(final T x) {
                if(x.equals(item)) {
                    ret.value = true;
                    return Go.Break;
                } else {
                    return Go.Continue;
                }
            }
        });
        return ret.value;
    }
    public T findWhere(final F<T, Boolean> where) {
        final Mut<T> ret = new Mut<T>(null);
        goOn(new F<T, Go>() {
            @Override
            public Go apply(final T x) {
                if(where.apply(x)) {
                    ret.value = x;
                    return Go.Break;
                } else {
                    return Go.Continue;
                }
            }
        });
        return ret.value;
    }
    public boolean existsWhere(final F<T, Boolean> where) {
        final Mut<Boolean> ret = new Mut<Boolean>(false);
        goOn(new F<T, Go>() {
            @Override
            public Go apply(final T x) {
                if(where.apply(x)) {
                    ret.value = true;
                    return Go.Break;
                } else {
                    return Go.Continue;
                }
            }
        });
        return ret.value;
    }
    public boolean allConfirm(final F<T, Boolean> confirm) {
        final Mut<Boolean> ret = new Mut<Boolean>(true);
        goOn(new F<T, Go>() {
            @Override
            public Go apply(final T x) {
                if(!(confirm.apply(x))) {
                    ret.value = false;
                    return Go.Break;
                } else {
                    return Go.Continue;
                }
            }
        });
        return ret.value;
    }
    public T head() {
        final Mut<T> ret = new Mut<T>();
        goOn(new F<T, Go>() {
            @Override
            public Go apply(final T on) {
                ret.value = on;
                return Go.Break;
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