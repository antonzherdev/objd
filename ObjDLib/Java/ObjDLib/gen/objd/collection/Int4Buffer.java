package objd.collection;

import objd.lang.*;
import java.nio.IntBuffer;

public class Int4Buffer<T> extends Buffer<Integer> {
    public final IntBuffer buffer;
    @Override
    public IntBuffer buffer() {
        return buffer;
    }
    public int get() {
        return this.buffer.get();
    }
    public void setV(final int v) {
        this.buffer.put(v);
    }
    public Int4Buffer(final int count) {
        super(count);
        this.buffer = IntBuffer.allocate(((int)(count)));
    }
    public String toString() {
        return "Int4Buffer";
    }
}