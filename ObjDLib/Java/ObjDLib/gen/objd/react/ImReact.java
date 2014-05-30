package objd.react;

import objd.lang.*;

public abstract class ImReact<T> extends React<T> {
    @Override
    public void attachObserver(final Observer<T> observer) {
    }
    @Override
    public void detachObserver(final Observer<T> observer) {
    }
    public ImReact() {
    }
    public String toString() {
        return "ImReact";
    }
}