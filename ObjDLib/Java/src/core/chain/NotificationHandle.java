package core.chain;

public class NotificationHandle<S, D> {
    public final String name;
    public void postSender(S sender) {
        ERROR: Unknown <to>NotificationCenter\NotificationCenter#C.class\.<eIt>instance\NotificationCenter#C\.<dI>post(name = <NotificationHandle#C<S#G, D#G>>self.<eIU>name\string\, sender = <l>sender\§S#G§\, data = nil)\void\;
    }
    public void postSenderData(S sender,D data) {
        ERROR: Unknown <to>NotificationCenter\NotificationCenter#C.class\.<eIt>instance\NotificationCenter#C\.<dI>post(name = <NotificationHandle#C<S#G, D#G>>self.<eIU>name\string\, sender = <l>sender\§S#G§\, data = <l>data\§D#G§\)\void\;
    }
    public NotificationObserver observeBy(F2<S, D, Void> by) {
        return ERROR: Unknown <to>NotificationCenter\NotificationCenter#C.class\.<eIt>instance\NotificationCenter#C\.<dI>addObserver(name = <NotificationHandle#C<S#G, D#G>>self.<eIU>name\string\, sender = nil, block = sender : §S#G§, data : §D#G§ -> void = <lw>by\(§S#G§, §D#G§) -> void\.<d>apply( = <l>sender\§S#G§\,  = <l>data\§D#G§\)\void\)\NotificationObserver#C\;
    }
    public NotificationObserver observeSenderBy(S sender,F<D, Void> by) {
        return ERROR: Unknown <to>NotificationCenter\NotificationCenter#C.class\.<eIt>instance\NotificationCenter#C\.<dI>addObserver(name = <NotificationHandle#C<S#G, D#G>>self.<eIU>name\string\, sender = <l>sender\§S#G§\, block = _ : §S#G§, data : §D#G§ -> void = <lw>by\§D#G§ -> void\.<d>apply( = <l>data\§D#G§\)\void\)\NotificationObserver#C\;
    }
    public NotificationHandle(String name) {
    }
    static final ClassType<NotificationHandle<S, D>> type;
}