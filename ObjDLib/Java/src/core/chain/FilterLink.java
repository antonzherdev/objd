package core.chain;

public class FilterLink<T> implements ChainLink<T, T> {
    public F<T, Boolean> predicate;
    public float selectivity;
    public Yield<T> buildYield(Yield<T> yield) {
    }
    public FilterLink(F<T, Boolean> predicate,float selectivity) {
    }
    static ClassType<FilterLink<T>> type;
}