package objd.collection;

import objd.lang.*;
import java.nio.FloatBuffer;

public class Float4Buffer<T> extends Buffer<Float> {
    public final FloatBuffer buffer;
    @Override
    public FloatBuffer buffer() {
        return buffer;
    }
    public int get() {
        return ((int)(this.buffer.get()));
    }
    public void setV(final int v) {
        this.buffer.put(((float)(v)));
    }
    public Float4Buffer(final int count) {
        super(count);
        this.buffer = FloatBuffer.allocate(((int)(count)));
    }
    public String toString() {
        return "Float4Buffer";
    }
}