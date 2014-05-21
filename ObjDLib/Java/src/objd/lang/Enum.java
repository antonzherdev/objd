package objd.lang;

import objd.lang.*;

public class Enum implements Comparable<Enum> {
    public static abstract ImArray<Enum> values();
    public final int ordinal;
    public final String name;
    @Override
    public String toString() {
        return this.name;
    }
    @Override
    public int hashCode() {
        return this.ordinal;
    }
    @Override
    public int compareTo(final Enum to) {
        return this.ordinal.compareTo(to.ordinal);
    }
    public Enum(final int ordinal, final String name) {
        this.ordinal = ordinal;
        this.name = name;
    }
}