package core.chain;

public class ShuffleLink<T> implements ChainLink<T, T> {
    private MArray<T> _array;
    public Yield<T> buildYield(Yield<T> yield) {
    }
    public ShuffleLink() {
    }
    static ClassType<ShuffleLink<T>> type;
}