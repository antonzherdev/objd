package objd.collection;

import objd.lang.*;

public abstract class Buffer<T> {
    public abstract java.nio.Buffer buffer();
    public final int count;
    public void reset() {
        this.buffer().clear();
    }
    public Buffer(final int count) {
        this.count = count;
    }
    public String toString() {
        return String.format("Buffer(%d)", this.count);
    }
}