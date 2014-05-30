package objd.lang;

@SuppressWarnings("unchecked")
public class Util {
    public static <T> T as(Class<?> cls, Object obj) {
        return cls.isInstance(obj) ? (T)obj : null;
    }
}
