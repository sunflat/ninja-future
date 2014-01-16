/**
 * @author iwasaki
 */
import scala.util.Try
import scala.util.Success
import scala.util.Failure

object TryExercise {
  import Main._
  
  implicit class MapForTry[A, B](val map:Map[A,B]) extends AnyVal{
    def getTry(key:A):Try[B] = Try(map(key))
  }
  
  class ConversationTry(
    affinity: Map[(User, User), Emotion],
    message: Map[(User, Emotion), String]
  ) {
    def generate_simple(u1: User, u2: User): Try[String] = {
      for(emotion <- affinity.getTry((u1,u2)); msg <- message.getTry((u1,emotion))) yield template(u1,u2,msg)
    }

    def generate_complex(u1: User, u2: User): Try[String] = {
      for{emotion1 <- affinity.getTry((u1, u2))
          emotion2 <- affinity.getTry((u2, u1)) if (emotion1 == emotion2)
          msgStr <- message.getTry((u1, inverse(emotion1))).orElse(message.getTry((u1, emotion1)))
      } yield template(u1, u2, msgStr)
    }
  }
}
