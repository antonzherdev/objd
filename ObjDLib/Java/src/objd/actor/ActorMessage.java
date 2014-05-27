package objd.actor;

import objd.lang.*;

public interface ActorMessage {
    Actor receiver();
    boolean prompt();
    boolean process();
    String toString();
}