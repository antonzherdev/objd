package core.chain;

public class NotificationHandle<S, D> {
    public final String name;
    public void postSender(S sender) {
        NotificationCenter().instance.postNameSenderData<S, Object>(name, sender, ERROR: Unknown nil);
    }
    public void postSenderData(S sender,D data) {
        NotificationCenter().instance.postNameSenderData<S, D>(name, sender, data);
    }
    public NotificationObserver observeBy(P2<S, D> by) {
        return NotificationCenter().instance.addObserverNameSenderBlock<S, D>(name, ERROR: Unknown nil, new P2<S, D>() {
            @Override
            public void f(S sender,D data) {
                by.apply(sender, data);
            }
        });
    }
    public NotificationObserver observeSenderBy(S sender,P<D> by) {
        return NotificationCenter().instance.addObserverNameSenderBlock<S, D>(name, sender, new P2<S, D>() {
            @Override
            public void f(S _,D data) {
                by.apply(data);
            }
        });
    }
    public NotificationHandle(String name) {
    }
}