package core.chain;

public class NotificationHandle<S, D> {
    public final String name;
    public void postSender(final S sender) {
        NotificationCenter.instance.postNameSenderData<S, Object>(this.name, sender, null);
    }
    public void postSenderData(final S sender, final D data) {
        NotificationCenter.instance.postNameSenderData<S, D>(this.name, sender, data);
    }
    public NotificationObserver observeBy(final P2<S, D> by) {
        return NotificationCenter.instance.addObserverNameSenderBlock<S, D>(this.name, null, new P2<S, D>() {
            @Override
            public void apply(final S sender, final D data) {
                by.apply(sender, data);
            }
        });
    }
    public NotificationObserver observeSenderBy(final S sender, final P<D> by) {
        return NotificationCenter.instance.addObserverNameSenderBlock<S, D>(this.name, sender, new P2<S, D>() {
            @Override
            public void apply(final S _, final D data) {
                by.apply(data);
            }
        });
    }
    public NotificationHandle(final String name) {
        this.name = name;
    }
}