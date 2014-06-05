package objd.collection;

import objd.lang.*;
import java.nio.FloatBuffer;

public class Float4Buffer<T> extends Buffer<Float> {
    public final FloatBuffer bytes;
    @Override
    public FloatBuffer bytes() {
        return bytes;
    }
    public int get() {
        return ((int)(this.bytes.get()));
    }
    public void setV(final int v) {
        this.bytes.put(((float)(v)));
    }
    public Float4Buffer(final int count) {
        super(count, ((int)(4)));
        this.bytes = FloatBuffer.allocate(((int)(count)));
    }
    public String toString() {
        return "Float4Buffer";
    }
}