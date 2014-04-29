package objd.collection;

public interface Traversable<T> {
    void forEach(final P<T> each);
    void parForEach(final P<T> each);
    boolean goOn(final F<T, Boolean> on);
    Chain<T> chain();
    T findWhere(final F<T, Boolean> where);
    boolean existsWhere(final F<T, Boolean> where);
    boolean allConfirm(final F<T, Boolean> confirm);
    T head();
    <C extends Traversable<T>> C convertWithBuilder(final Builder<T, C> builder);
}