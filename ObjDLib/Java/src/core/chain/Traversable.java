package core.chain;

public interface Traversable<T> {
    void forEach(P<T> each);
    void parForEach(P<T> each);
    boolean goOn(F<T, Boolean> on);
    Chain<T> chain();
    T findWhere(F<T, Boolean> where);
    boolean existsWhere(F<T, Boolean> where);
    boolean allConfirm(F<T, Boolean> confirm);
    T head();
     <C extends Traversable<T>> C convertWithBuilder(Builder<T, C> builder);
}