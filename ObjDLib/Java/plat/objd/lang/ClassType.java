package objd.lang;

public class ClassType<T> {
    private final Class<T> cls;

    public ClassType(Class<T> cls) {
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
