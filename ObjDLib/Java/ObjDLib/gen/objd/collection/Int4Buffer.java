package objd.collection;

import objd.lang.*;
import java.nio.IntBuffer;

public class Int4Buffer<T> extends Buffer<Integer> {
    public final IntBuffer bytes;
    @Override
    public IntBuffer bytes() {
        return bytes;
    }
    public int get() {
        return this.bytes.get();
    }
    public void setV(final int v) {
        this.bytes.put(v);
    }
    public Int4Buffer(final int count) {
        super(count, ((int)(4)));
        this.bytes = IntBuffer.allocate(((int)(count)));
    }
    public String toString() {
        return "Int4Buffer";
    }
}