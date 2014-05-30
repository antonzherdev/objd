package objd.react;

public class Signal<T> extends ObservableBase_impl<T> {
    public void postData(final T data) {
        notifyValue(data);
    }
    public void post() {
        ((Signal<Void>)(((Signal)(this)))).notifyValue(null);
    }
    public Signal() {
    }
    public String toString() {
        return "Signal";
    }
}