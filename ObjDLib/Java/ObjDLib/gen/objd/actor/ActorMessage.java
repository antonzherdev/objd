package objd.actor;

public interface ActorMessage {
    Actor receiver();
    boolean prompt();
    boolean process();
    String toString();
}