package core.chain;

public class NotificationHandle<S, D> {
    public String name;
    public void postSender(S sender) {
    }
    public void postSenderData(S sender,D data) {
    }
    public NotificationObserver observeBy(F2<S, D, Void> by) {
    }
    public NotificationObserver observeSenderBy(S sender,F<D, Void> by) {
    }
    public NotificationHandle(String name) {
    }
    static ClassType<NotificationHandle<S, D>> type;
}