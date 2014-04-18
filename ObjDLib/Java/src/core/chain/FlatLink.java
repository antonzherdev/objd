package core.chain;

public class FlatLink<T> implements ChainLink<Traversable<T>, T> {
    public float factor;
    public Yield<Traversable<T>> buildYield(Yield<T> yield) {
    }
    public FlatLink(float factor) {
    }
    static ClassType<FlatLink<T>> type;
}