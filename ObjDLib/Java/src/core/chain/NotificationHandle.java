package core.chain;

public class NotificationHandle<S, D> {
    public final String name;
    public void postSender(S sender) {
        NotificationCenter().instance.post<S, Object>(this.name, sender, null);
    }
    public void postSenderData(S sender,D data) {
        NotificationCenter().instance.post<S, D>(this.name, sender, data);
    }
    public NotificationObserver observeBy(P2<S, D> by) {
        return NotificationCenter().instance.addObserver<S, D>(this.name, null, new P2<S, D>() {
            @Override
            public void apply(S sender,D data) {
                by.apply(sender, data);
            }
        });
    }
    public NotificationObserver observeSenderBy(S sender,P<D> by) {
        return NotificationCenter().instance.addObserver<S, D>(this.name, sender, new P2<S, D>() {
            @Override
            public void apply(S _,D data) {
                by.apply(data);
            }
        });
    }
    public NotificationHandle(String name) {
        this.name = name;
    }
}