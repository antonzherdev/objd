package objd.collection;

import objd.lang.*;

public abstract class Buffer<T> {
    public abstract java.nio.Buffer bytes();
    public final int count;
    public final int stride;
    public final int length;
    public void reset() {
        this.bytes().clear();
    }
    public Buffer(final int count, final int stride) {
        this.count = count;
        this.stride = stride;
        this.length = stride * count;
    }
    public String toString() {
        return String.format("Buffer(%d, %d)", this.count, this.stride);
    }
}