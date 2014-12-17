package objd.collection;

import objd.lang.*;
import java.nio.FloatBuffer;

public class Float4Buffer extends Buffer<Float> {
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
    public void forF(final P<Float> f) {
        int i = 0;
        this.bytes.clear();
        while(i < this.count) {
            f.apply(((float)(((int)(this.bytes.get())))));
            i++;
        }
    }
    public Float4Buffer(final int count) {
        super(count, ((int)(4)));
        this.bytes = FloatBuffer.allocate(((int)(count)));
    }
    public String toString() {
        return "Float4Buffer";
    }
}