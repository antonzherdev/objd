package objd.collection;

import objd.lang.*;
import objd.chain.Chain;

public interface Traversable<T> {
    void forEach(final P<T> each);
    void parForEach(final P<T> each);
    Go goOn(final F<T, Go> on);
    Chain<T> chain();
    T findWhere(final F<T, Boolean> where);
    boolean existsWhere(final F<T, Boolean> where);
    boolean allConfirm(final F<T, Boolean> confirm);
    T head();
    <C extends Traversable<T>> C convertWithBuilder(final Builder<T, C> builder);
}