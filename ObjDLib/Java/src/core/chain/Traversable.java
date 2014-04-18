package core.chain;

public interface Traversable<T> {
    void forEach(F<T, Void> each);
    void parForEach(F<T, Void> each);
    boolean goOn(F<T, Boolean> on);
    Chain<T> chain();
    T findWhere(F<T, Boolean> where);
    boolean existsWhere(F<T, Boolean> where);
    boolean allConfirm(F<T, Boolean> confirm);
    T head();
    C convertWithBuilder(Builder<T, C> builder);
}