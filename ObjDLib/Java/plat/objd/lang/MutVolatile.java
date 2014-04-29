package objd.lang;

public class MutVolatile<T> {
    public volatile T value;

    public MutVolatile() {
    }
    public MutVolatile(T value) {
        this.value = value;
    }
}
