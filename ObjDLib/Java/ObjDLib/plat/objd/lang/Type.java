package objd.lang;

public class Type<T> {
    protected final Class<T> cls;

    public Type(Class<T> cls) {
        this.cls = cls;
    }

    public String getName() {
        return cls.getName();
    }

    public Class<T> getCls() {
        return cls;
    }

    public boolean isInstanceObj(Object obj) {
        return cls.isInstance(obj);
    }
}
